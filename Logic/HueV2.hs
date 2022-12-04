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
import Data.Text (Text,unpack,pack,isInfixOf,toCaseFold)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))
import Logic.Common
import Types
import Data.Hashable
import Data.LargeHashable (Word128(..))
import Data.LargeHashable.Class
import Config
import Data.Bits


-- zigDevToId :: MQTT.ZigDevice -> Identifier
-- zigDevToId ZigDevice{network_address,ieee_address=IEEEAddress w}
--   = Identifier (Word128 w (fromIntegral network_address))

hashableToId :: LargeHashable a => a -> Identifier
hashableToId = Identifier . hash128

bridge :: NetConfig -> BridgeGet
bridge cfg = BridgeGet
    {_id = hashableToId (macContents (mac cfg))
    ,id_v1 = ""
    ,owner = Nothing
    ,bridge_id = mkBridgeIdLower cfg
    ,time_zone = TimeZone "Europe/Stockholm" -- FIXME: config
    ,_type = Bridge
    }  


mkLightDevice :: ResourceRef -> Int -> ZigDevice -> Device -> DeviceGet
mkLightDevice lightRef v1Id ZigDevice{..} MQTT.Device{name=prodname,..} = DeviceGet
  { _id = hashableToId ieee_address
  , id_v1 = "/light/" <> pack (show v1Id) 
  , product_data = ProductData
    { certified = False
    , software_version = sw_version
    , hardware_platform_type = "1166-116" -- Innr, FIXME
    , model_id = fromMaybe "ABC123" model_id
    , manufacturer_name = manufacturer
    , product_name = prodname
    , product_archetype = ClassicBulb -- FIXME
    }
  , metadata = ArchetypeMeta
    { name = friendly_name
    , archetype = SultanBulb -- FIXME
    }
  , services = [lightRef]
  , _type = HueAPIV2.Device
  }

mkLightService v1Id owner fname
  LightConfig{..}
  LightState{brightness=bri,state,color,color_temp,color_mode=mode} = LightGet
  {_id = identStore (hashableToId unique_id) v1Id --  Identifier,
  ,id_v1 = "/light/" <> pack (show v1Id) --  Path,
  ,owner = owner --  ResourceRef,       -- owner is light device
  ,metadata = ArchetypeMeta
              { name = fname
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


-- mkLight 
--   {
--       "id": "bca61a7f-0471-4ac0-9500-0a43d291179a",
--       "id_v1": "/lights/7",
--       "owner": {
--         "rid": "6cc8c1fc-13d0-4f26-aafb-e2f8939fcd6a",
--         "rtype": "device"
--       },
--       "metadata": {
--         "name": "Gledopto controller Wardrobe",
--         "archetype": "ceiling_round"
--       },
--       "on": {
--         "on": true
--       },
--       "dimming": {
--         "brightness": 100.0
--       },
--       "dimming_delta": {},
--       "color_temperature": {
--         "mirek": 366,
--         "mirek_valid": true,
--         "mirek_schema": {
--           "mirek_minimum": 155,
--           "mirek_maximum": 495
--         }
--       },
--       "color_temperature_delta": {},
--       "color": {
--         "xy": {
--           "x": 0.4584,
--           "y": 0.41
--         }
--       },
--       "dynamics": {
--         "status": "none",
--         "status_values": [
--           "none"
--         ],
--         "speed": 0.0,
--         "speed_valid": false
--       },
--       "alert": {
--         "action_values": [
--           "breathe"
--         ]
--       },
--       "signaling": {},
--       "mode": "normal",
--       "type": "light"
--     }

-- {
--       "id": "c2b42ff1-9e26-4ca1-9891-648dec038994",
--       "id_v1": "/groups/9",
--       "owner": {
--         "rid": "d8639195-dbe2-41a2-8a53-1d7b3930ae58",
--         "rtype": "room"
--       },
--       "on": {
--         "on": false
--       },
--       "dimming": {
--         "brightness": 0.0
--       },
--       "dimming_delta": {},
--       "color_temperature": {},
--       "color_temperature_delta": {},
--       "alert": {
--         "action_values": [
--           "breathe"
--         ]
--       },
--       "signaling": {},
--       "dynamics": {},
--       "type": "grouped_light"
--     }

-- mkRoom ::  AppState -> GroupConfig -> GroupGet
-- mkRoom  st@AppState{..} g@GroupConfig{..} = GroupGet
--   {_id = identStore (hashableToId friendly_name) _id
--   ,id_v1 = "/groups/" <> pack (show _id)
--   ,children = [ResourceRef (mkDeviceId address) HueAPIV2.Device ] -- devices
--   ,services = [] -- one grouped light for the exact same thing.
--   ,metadata = ArchetypeMeta {name = friendly_name
--                             ,archetype = case () of
--                                 _ | "office" `isInfixOf` nm -> Office
--                                 _ | "bedroom" `isInfixOf` nm -> Bedroom
--                                 _ | "garage" `isInfixOf` nm -> Garage
--                                 _ | "hallway" `isInfixOf` nm -> Hallway
--                                 _ | "wardrobe" `isInfixOf` nm -> Hallway
--                                 _ | "kitchen" `isInfixOf` nm -> Kitchen
--                                 _ | "attic" `isInfixOf` nm -> Attic
--                                 _ | "living" `isInfixOf` nm -> LivingRoom
--                                 _ -> Bedroom
--                             }
--   ,_type = Room
--   } where
--         nm = toCaseFold friendly_name
{-

getHueGroups :: AppState -> [GroupGet]
getHueGroups st@AppState{groups} = [mkRoom i g | (i,g) <- Map.assocs groups ]

-- mkGroupWithLights :: AppState -> GroupType -> Text -> Map IEEEAddress MQTT.LightConfig -> Group
-- mkGroupWithLights st@AppState{..} _type name ls
--   = Group {lights = [pack (show i)
--                     | (uid,_) <- Map.toList ls
--                     , let Just i = Map.lookup uid lightIds]
--           ,sensors = mempty
--           ,state = GroupState {all_on = and ons
--                               ,any_on = or ons}
--           ,recycle = False
--           ,_class = case () of
--               _ | "office" `isInfixOf` nm -> Just Office
--               _ | "bedroom" `isInfixOf` nm -> Just Bedroom
--               _ | "garage" `isInfixOf` nm -> Just Garage
--               _ | "hallway" `isInfixOf` nm -> Just Hallway
--               _ | "wardrobe" `isInfixOf` nm -> Just Hallway
--               _ | "kitchen" `isInfixOf` nm -> Just Kitchen
--               _ | "attic" `isInfixOf` nm -> Just Attic
--               _ | "living" `isInfixOf` nm -> Just LivingRoom
--               _ -> Just Bedroom
--           ,action = lightStateMqtt2Hue (combineLightStates groupLightStates)
--           ,..}
--   where ons = [state == ON | MQTT.LightState{state} <- groupLightStates  ]
--         groupLightStates = getLightState st . snd <$> Map.toList ls
--         nm = toCaseFold name


-- groups :: AppState -> 
-- {
--       "id": "581d7d2d-8cdc-4462-99d4-79182f44b63a",
--       "id_v1": "/groups/1",
--       "children": [
--         {
--           "rid": "01409ea5-3435-4be6-9698-469021626b26",
--           "rtype": "device"
--         },
--         {
--           "rid": "b372bc87-6fe5-4b1d-a2c7-294628874dd0",
--           "rtype": "device"
--         }
--       ],
--       "services": [
--         {
--           "rid": "1c99d52a-b4e6-44ff-9331-61cfcbd678eb",
--           "rtype": "grouped_light"
--         }
--       ],
--       "metadata": {
--         "name": "JP office",
--         "archetype": "office"
--       },
--       "type": "room"
--     }
-}
