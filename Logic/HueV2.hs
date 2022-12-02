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

zigDevToId :: MQTT.ZigDevice -> Identifier
zigDevToId ZigDevice{network_address,ieee_address=IEEEAddress w}
  = Identifier (Word128 w (fromIntegral network_address))

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


{-
-- device 
mkRoomWithLights ::  AppState -> -> [MQTT.LightConfig] -> GroupGet
mkRoomWithLights  st@AppState{..}  ls = GroupGet
  {_id = identStore (hashableToId name)
  ,id_v1 = "/groups/" <> _
  ,children = [_] -- devices
  ,services = []
  ,metadata = ArchetypeMeta {name = friendly_name
                            ,archetype = case () of
                                _ | "office" `isInfixOf` nm -> "office"
                                _ | "bedroom" `isInfixOf` nm -> "bedroom"
                                _ | "garage" `isInfixOf` nm -> "garage"
                                _ | "hallway" `isInfixOf` nm -> "hallway"
                                _ | "wardrobe" `isInfixOf` nm -> "hallway"
                                _ | "kitchen" `isInfixOf` nm -> "kitchen"
                                _ | "attic" `isInfixOf` nm -> "attic"
                                _ | "living" `isInfixOf` nm -> "living_room"
                            }
  ,_type = Room
  } where
        nm = toCaseFold friendly_name

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
