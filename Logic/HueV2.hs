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

mkBridge :: ServerConfig -> AppState -> (DeviceGet,BridgeGet)
mkBridge ServerConfig{timezone} AppState{configuration=cfg} = (device,bridge) where
  devId = hashableToId ("Zigbee Controller"::Text)
  deviceRef = ResourceRef{rid=devId, rtype=DeviceResource} 
  bridgeId = hashableToId (macContents (mac cfg))
  device = DeviceGet
    {_id = devId
    ,id_v1 = ""
    ,product_data = ProductData
      {model_id = mkModelId
      ,manufacturer_name = bridgeManufacturerName
      ,product_name = bridgeProductName
      ,product_archetype = BridgeV2
      ,certified = True
      ,software_version = mkSoftwareVersion
      ,hardware_platform_type = Nothing}
    ,metadata = ArchetypeMeta
      {name = bridgeProductName
      ,archetype = BridgeV2}
    ,services = [ ResourceRef bridgeId Bridge ]
    ,_type = DeviceResource
    }
  bridge = BridgeGet
      {_id = bridgeId
      ,id_v1 = ""
      ,owner = Just deviceRef
      ,bridge_id = mkBridgeIdLower cfg
      ,time_zone = TimeZone timezone
      ,_type = Bridge
      }  

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
      { certified = True
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
                                   ,min_dim_level = Nothing -- FIXME
                                   }) bri
    ,color_temperature =
     if TemperatureMode `elem` supported_color_modes
       then
        Just ColorTempGet
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



mkBridgeHome :: AppState -> (GroupGet, LightGet)
mkBridgeHome st@AppState{..} = mkGroup 0 "# Home group #" BridgeHome ls as groupLightState Nothing
  where as = Map.keys lights
        ls = Map.elems lights
        groupLightState = combineLightStates (getLightState st <$> ls)

mkGroup :: Int -> Text -> ResourceType -> [LightConfig]
        -> [IEEEAddress] -> LightState -> Maybe ArchetypeMeta -> (GroupGet, LightGet)
mkGroup gid friendly_name rtype lightCfgs lAddrs groupLightState metadata = (grp,light) where
  groupedLightId = identStore (hashableToId friendly_name) gid
  roomId = hashableToId (friendly_name,gid)
  roomRef = ResourceRef roomId rtype
  id_v1 = "/groups/" <> pack (show gid)
  light = mkLightService friendly_name groupedLightId id_v1 roomRef (cmode,mmin,mmax) groupLightState
  cmode = nub $ concat (supported_color_modes <$> lightCfgs)
  mmin = case catMaybes (min_mireds <$> lightCfgs) of
    [] -> Nothing
    xs -> Just (maximum xs)
  mmax = case catMaybes (max_mireds <$> lightCfgs) of
    [] -> Nothing
    xs -> Just (minimum xs)
  grp = GroupGet
   {_id = roomId
   ,children = [mkDeviceRef a | a <- lAddrs] -- devices
   ,services = [ResourceRef groupedLightId GroupedLight]
   ,_type = rtype
   ,..
   }

mkRoom :: AppState -> GroupConfig -> (GroupGet, LightGet)
mkRoom AppState{..} g@GroupConfig{_id=gid,..} =
  mkGroup gid friendly_name Room lightCfgs memberAddresses groupLightState metadata
 where
  memberAddresses = [a | GroupMember{ieee_address=a} <- members]
  lightCfgs = catMaybes [Map.lookup a lights | a <- memberAddresses]
  groupLightState = fromMaybe blankLightState (Map.lookup (groupNotifyTopic g) lightStates )
  metadata = Just $ ArchetypeMeta {name = friendly_name
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
  nm = toCaseFold friendly_name

mkRooms :: AppState -> [(GroupGet, LightGet)]
mkRooms st@AppState{groups} = mkRoom st <$> Map.elems groups
  
mkResources :: ServerConfig -> AppState -> [ResourceGet]
mkResources cfg st@AppState{..} = brs ++ lrs ++ rrs ++ [RGeoLoc mkGeoLoc, RGroup hg, RLight hl]
  where
  brs = [RDevice d, RBridge b] where (d,b) = mkBridge cfg st
  (hg,hl) = mkBridgeHome st
  lrs = concat [[RDevice dr, RLight lr]
               | (a,lc) <- Map.assocs lights
               , Just i <- [Map.lookup a lightIds]
               , Just z <- [Map.lookup a zigDevices]
               , Just ls <- [Map.lookup (lightNotifyTopic lc) lightStates]
               , let (dr,lr) = mkLight i z lc ls ]
  rrs = concat [[RGroup gr, RLight lr] | (gr,lr) <- mkRooms st]

mkLights :: ServerConfig -> AppState -> [LightGet]
mkLights cfg st = [l | RLight l <- mkResources cfg st] 

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
  ,color_temp = fmap (\ColorTempSet{mirek} -> mirek) color_temperature
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

translateGroupAction :: Identifier -> LightPut -> AppState -> AgendaItem
translateGroupAction i a0 AppState{groups} = mkGroupAction l a
 where a = actionHue2Mqtt a0
       l = case Map.lookup gid groups of
         Nothing -> error ("translate group: unknown gid" <> show gid)
         Just x -> x         
       gid = identLoad i
