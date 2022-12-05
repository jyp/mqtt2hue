{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Logic.HueV2 where
 
import Data.Maybe
import MQTTAPI as MQTT
import HueAPIV2
import qualified Data.Map as Map
import Data.Map (Map,toList)
import Data.Text (Text,pack,isInfixOf,toCaseFold)
import Logic.Common
import Types
import Config
import Data.List (nub)

hashableToId :: LargeHashable a => a -> Identifier
hashableToId = Identifier . hash128

mkBridge :: AppState -> (DeviceGet,BridgeGet)
mkBridge AppState{configuration=cfg} = (device,bridge) where
  devId = hashableToId ("Zigbee Controller"::Text)
  deviceRef = ResourceRef{rid=devId, rtype=DeviceResource} 
  bridgeId = hashableToId (macContents (mac cfg))
  device = DeviceGet
    {_id = devId
    ,id_v1 = ""
    ,product_data = ProductData
      {model_id = "BSB002"
      ,manufacturer_name = "Signify Netherlands B.V."
      ,product_name = "Philips hue"
      ,product_archetype = BridgeV2
      ,certified = False
      ,software_version = "1.53.1953188020"
      ,hardware_platform_type = Nothing}
    ,metadata = ArchetypeMeta {name = "MQTT2Hue"
                              ,archetype = BridgeV2}
    ,services = [ ResourceRef bridgeId Bridge ]
    ,_type = DeviceResource
    }
  bridge = BridgeGet
      {_id = bridgeId
      ,id_v1 = ""
      ,owner = Just deviceRef
      ,bridge_id = mkBridgeIdLower cfg
      ,time_zone = TimeZone "Europe/Stockholm" -- FIXME: config
      ,_type = Bridge
      }  

{-{
      "id": "69dc4fd9-01e7-4329-ae0b-476ad339753c",
      "id_v1": "",
      "product_data": {
        "model_id": "BSB002",
        "manufacturer_name": "Signify Netherlands B.V.",
        "product_name": "Philips hue",
        "product_archetype": "bridge_v2",
        "certified": true,
        "software_version": "1.53.1953188020"
      },
      "metadata": {
        "name": "Philips hue",
        "archetype": "bridge_v2"
      },
      "identify": {},
      "services": [
        {
          "rid": "1546cfb2-70a3-4b6c-a233-9ac38c7d4980",
          "rtype": "bridge"
        },
        {
          "rid": "047aeeaf-8b88-4c78-893f-73babfa3f1af",
          "rtype": "zigbee_connectivity"
        },
        {
          "rid": "e996e8b7-854f-4fee-a879-37f161e0bbd0",
          "rtype": "entertainment"
        }
      ],
      "type": "device"
    }-}

mkDeviceRef :: IEEEAddress -> ResourceRef
mkDeviceRef addr = ResourceRef (hashableToId addr) DeviceResource


mkLight :: Int -> ZigDevice -> LightConfig -> LightState -> (DeviceGet, LightGet)
mkLight v1Id zdev@ZigDevice{ieee_address,friendly_name}
        l@LightConfig{unique_id,device,supported_color_modes,max_mireds,min_mireds} ls =
  (mkLightDevice zdev device, svc) where
  serviceId = identStoreAddr (hashableToId unique_id) ieee_address
  deviceRef@ResourceRef{rid = deviceId} = mkDeviceRef ieee_address
  mkLightDevice :: ZigDevice -> Device -> DeviceGet
  mkLightDevice  ZigDevice{model_id} MQTT.Device{name=prodname,..} = DeviceGet
    { _id = deviceId
    , id_v1 = "/light/" <> pack (show v1Id) 
    , product_data = ProductData
      { certified = False
      , software_version = sw_version
      , hardware_platform_type = Nothing -- "1166-116" -- Innr, FIXME
      , model_id = fromMaybe "ABC123" model_id
      , manufacturer_name = manufacturer
      , product_name = prodname
      , product_archetype = ClassicBulb -- FIXME
      }
    , metadata = ArchetypeMeta
      { name = friendly_name
      , archetype = SultanBulb -- FIXME
      }
    , services = [ResourceRef serviceId LightResource]
    , _type = DeviceResource
    }
  path = "/light/" <> pack (show v1Id)
  svc = mkLightService friendly_name serviceId path deviceRef
                       (supported_color_modes,min_mireds,max_mireds) ls

mkLightService :: Text
               -> Identifier
               -> Path
               -> ResourceRef
               -> ([ColorMode], Maybe Int, Maybe Int)
               -> LightState
               -> LightGet
mkLightService friendly_name serviceId path owner
    (supported_color_modes,min_mireds,max_mireds)
    LightState{brightness=bri,state,color,color_temp,color_mode=mode} = LightGet
    {_id = serviceId
    ,id_v1 = path --  Path,
    ,owner = owner
    ,metadata = ArchetypeMeta
                { name = friendly_name
                , archetype = SultanBulb -- FIXME
                }
    ,on = IsOn (state == ON) --  IsOn,
    ,dimming = fmap (\b -> Dimming{brightness = (100.0 / 254) * fromIntegral b
                                   ,min_dim_level = Nothing -- fixme
                                   }) bri
    ,color_temperature =
     if TemperatureMode `elem` supported_color_modes
       then
        Just ColorTemp
        {mirek = fromMaybe 200 color_temp
        ,mirek_valid = isJust color_temp && (mode == Just TemperatureMode)
        ,mirek_schema = MirekSchema
          {mirek_minimum = fromMaybe 153 min_mireds
          ,mirek_maximum = fromMaybe 450 max_mireds
          }
      } else Nothing
    ,color = if XYMode `elem` supported_color_modes
             then Just ColorGet
                  {xy = case color of
                      Just (ColorXY {x,y}) -> XY x y
                      Nothing -> XY 0.5 0.5
                  ,gamut = Nothing
                  ,gamut_type = Nothing
                  }
             else Nothing
    ,dynamics = Dynamics {status = "none"
                         ,status_values = ["none"]
                         ,speed = 0
                         ,speed_valid = False
                         } --  Dynamics,
    ,alert= Alert {action_values = ["breathe"]}
    ,mode = "normal" -- ???
    ,effects = Nothing
    ,_type = LightResource
    }

mkRoom :: AppState -> GroupConfig -> (GroupGet, LightGet)
mkRoom AppState{..} g@GroupConfig{_id=gid,..} = (room,light) where
  groupedLightId = identStore (hashableToId friendly_name) gid
  roomId = identStore (hashableToId friendly_name) gid
  roomRef = ResourceRef roomId Room
  light = mkLightService friendly_name groupedLightId ("/group/" <> pack (show gid)) roomRef (cmode,mmin,mmax) groupLightState
  memberAddresses = [a | GroupMember{ieee_address=a} <- members]
  lightCfgs = catMaybes [Map.lookup a lights | a <- memberAddresses]
  cmode = nub $ concat (supported_color_modes <$> lightCfgs)
  mmin = case catMaybes (min_mireds <$> lightCfgs) of
    [] -> Nothing
    xs -> Just (maximum xs)
  mmax = case catMaybes (max_mireds <$> lightCfgs) of
    [] -> Nothing
    xs -> Just (minimum xs)
  groupLightState = fromMaybe blankLightState (Map.lookup (groupNotifyTopic g) lightStates )
  room = GroupGet
   {_id = roomId
   ,id_v1 = "/groups/" <> pack (show gid)
   ,children = [mkDeviceRef a | a <- memberAddresses] -- devices
   ,services = [ResourceRef groupedLightId GroupedLight]
   ,metadata = ArchetypeMeta {name = friendly_name
                             ,archetype = case () of
                                 _ | "office" `isInfixOf` nm -> Office
                                 _ | "bedroom" `isInfixOf` nm -> Bedroom
                                 _ | "garage" `isInfixOf` nm -> Garage
                                 _ | "hallway" `isInfixOf` nm -> Hallway
                                 _ | "wardrobe" `isInfixOf` nm -> Hallway
                                 _ | "kitchen" `isInfixOf` nm -> Kitchen
                                 _ | "attic" `isInfixOf` nm -> Attic
                                 _ | "living" `isInfixOf` nm -> LivingRoom
                                 _ -> Bedroom
                             }
   ,_type = Room
   }
  nm = toCaseFold friendly_name

mkResources :: AppState -> [ResourceGet]
mkResources st@AppState{..} = brs ++ lrs ++ rrs ++ [RGeoLoc mkGeoLoc]
  where
  brs = [RDevice d, RBridge b] where (d,b) = mkBridge st
  lrs = concat [[RDevice dr, RLight lr]
               | (a,lc) <- Map.assocs lights
               , Just i <- [Map.lookup a lightIds]
               , Just z <- [Map.lookup a zigDevices]
               , Just ls <- [Map.lookup (lightNotifyTopic lc) lightStates]
               , let (dr,lr) = mkLight i z lc ls ]
  rrs = concat [[RGroup gr, RLight lr]
               | g <- Map.elems groups
               , let (gr,lr) = mkRoom st g ]

mkGeoFence :: GeoFenceGet
mkGeoFence = GeoFenceGet {_id = hashableToId ("geofence-client" :: Text)
                         ,name = "aiohue_myqiddfsyx"
                         ,_type = GeofenceClient
                         }

mkGeoLoc :: GeoLocationGet
mkGeoLoc = GeoLocationGet
  {_id = hashableToId ("geolocation" :: Text)
  ,is_configured = False
  ,_type = Geolocation
  }

actionHue2Mqtt  :: LightPut -> MQTT.Action
actionHue2Mqtt LightPut{..} = MQTT.Action {
  brightness = fmap (\Dimming {brightness} -> round (brightness*254/100)) dimming
  ,color = (<$> color) $ \case
      ColorGet {xy = XY x y} -> ColorXY x y Nothing Nothing
  ,state = (<$> on) $ \case
      IsOn True -> ON
      IsOn False -> OFF
  ,color_temp = fmap (\ColorTemp{mirek} -> mirek) color_temperature
  ,scene_recall = Nothing
  }

identStoreAddr :: Identifier -> IEEEAddress -> Identifier
identStoreAddr (Identifier (Word128 i _)) (IEEEAddress j)  = Identifier (Word128 i j)

identLoadAddr :: Identifier -> IEEEAddress
identLoadAddr (Identifier (Word128 _ i)) = IEEEAddress i


translateLightAction :: Identifier -> LightPut -> AppState -> AgendaItem
translateLightAction i a0 AppState{lights} = mkLightAction l a
 where a = actionHue2Mqtt a0
       l = case Map.lookup addr lights of
         Nothing -> error ("translate light: unknown addr" <> show addr)
         Just x -> x         
       addr = identLoadAddr i
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.
