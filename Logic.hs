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

module Logic where

import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map
import Data.Text (Text)
import Data.Time.Clock

blankServerState :: ServerState
blankServerState = ServerState mempty mempty mempty

data ServerState = ServerState {lights :: Map Text MQTTAPI.LightConfig -- map from uniqueid to config
                               ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state
                               ,lightIds :: Map Text Int -- map from uniqueid to simple id
                               }

lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {brightness,color_temp,state,color_mode,color}
  = HueAPI.LightState {on = state == ON
                      ,bri = brightness
                      -- ,hue = _ -- FIXME
                      -- ,sat = _
                      ,ct = color_temp
                      ,effect = None
                      ,xy = fmap (\(ColorXY x y) -> [x,y]) color
                      ,alert = Select
                      ,colorMode = (<$> color_mode) $ \case
                          TemperatureMode -> CT
                          XYMode -> XY
                      ,mode = HomeAutomation
                      ,reachable = True -- FIXME -- linkquality ?
                      }

lightMqtt2Hue :: MQTTAPI.LightConfig -> HueAPI.LightState -> HueAPI.Light
lightMqtt2Hue (MQTTAPI.LightConfig {device = Device {name=productname,..},..}) lightState
  = Light {state = lightState 
          ,swupdate = SwUpdate {state = NoUpdates
                               ,lastinstall = UTCTime (toEnum 0) (toEnum 0) -- FIXME
                               }
          ,_type = if XYMode `elem` supported_color_modes then
                     ExtendedColorLight else (if TemperatureMode `elem` supported_color_modes
                                              then TemperatureLight
                                              else DimmableLight)
          ,name = name
          ,modelid = model -- FIXME
          ,manufacturername = manufacturer
          ,productname = productname
          ,capabilities = Capabilities
            {certified = False,
             control = if XYMode `elem` supported_color_modes then
                     FullColor Other Nothing -- FIXME: Get gamut from manufacturer+modelid
                       else (if TemperatureMode `elem` supported_color_modes
                              then case (min_mireds,max_mireds) of
                                     (Just mmin, Just mmax) -> Ct (CtValues mmin mmax)
                                     _ -> NoControl
                              else NoControl)}
          ,config = HueAPI.LightConfig {
                         archetype = "sultanbulb",
                         function = "functional",
                         direction = "omnidirectional",
                         startup = Startup {mode = Safety, configured = True}}
          ,uniqueid = unique_id
          ,swversion = sw_version
          }


-- blankLightState :: HueAPI.LightState
-- blankLightState = HueAPI.LightState { on = True
--                                     , bri = 23
--                                     -- , hue = 44
--                                     -- , sat = 15
--                                     , ct = Nothing
--                                     , effect = None
--                                     , xy = Nothing
--                                     , alert = Select
--                                     , colorMode = Nothing
--                                     , mode = HomeAutomation
--                                     , reachable = True}

blankLightState :: MQTTAPI.LightState
blankLightState = MQTTAPI.LightState
  {brightness = 0
  ,color = Nothing
  ,color_mode = Nothing
  ,color_temp = Nothing
  ,linkquality = 0
  ,state = OFF
  ,update = UpdateState {state = MQTTAPI.Idle}
  ,update_available = False
  }

getLightState :: ServerState -> MQTTAPI.LightConfig -> MQTTAPI.LightState
getLightState ServerState{..} cfg =  Data.Map.findWithDefault blankLightState (state_topic cfg) lightStates

allHueLights :: ServerState -> Map Int Light
allHueLights st@ServerState{..} = Map.fromList
  [(i,lightMqtt2Hue cfg (lightStateMqtt2Hue (getLightState st cfg)))
  | (uid,cfg) <- toList lights
  , let Just i = Data.Map.lookup uid lightIds
  ]

allHueGroups :: ServerState -> Map Int Group
allHueGroups st = Map.fromList [(1,group1 st)]

mkGroupWithLights :: ServerState -> GroupType -> String -> Map Text MQTTAPI.LightConfig -> Group
mkGroupWithLights st@ServerState{..} _type name groupLights
  = Group {lights = [show i
                    | (uid,_) <- toList groupLights
                    , let Just i = Data.Map.lookup uid lightIds   ]
          ,sensors = mempty
          ,state = GroupState {all_on = and ons
                              ,any_on = or ons
                              }
          ,recycle = False
          ,_class = Nothing
          ,action = lightStateMqtt2Hue (head (groupLightStates++[blankLightState]))
          ,..}
  where ons = [state == ON | MQTTAPI.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> toList groupLights
  
group0 :: ServerState -> Group
group0 st@ServerState{lights} = mkGroupWithLights st LightGroup "Group 0" lights

group1 :: ServerState -> Group
group1 st@ServerState{lights} = mkGroupWithLights st Room "The Void" lights

updateLightConfig :: MQTTAPI.LightConfig -> ServerState -> ServerState
updateLightConfig l ServerState {..} =
  ServerState { lights = Data.Map.insert uid l lights
              , lightIds = if uid `member` lightIds then lightIds else insert uid (1 + maximum (0 : elems lightIds)) lightIds
              , ..}
  where uid = unique_id l

updateLightState :: Text -> MQTTAPI.LightState -> ServerState -> ServerState
updateLightState topic l ServerState {..} = ServerState {lightStates = Data.Map.insert topic l lightStates, ..}

