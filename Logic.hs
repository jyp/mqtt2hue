{-# LANGUAGE EmptyCase #-}
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

module Logic ( AppState(..),
              --  hue side
              allHueLights, allHueGroups, group0, handleLightAction,
              -- MQTT side
              blankAppState,  updateLightConfig, updateLightState
             ) where

import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map
import Data.Text (Text,splitOn,unpack)
import Data.Time.Clock
import Text.Read (readMaybe)

blankAppState :: AppState 
blankAppState = AppState mempty mempty mempty mempty mempty

data AppState = AppState
  {lights :: Map IEEEAddress MQTTAPI.LightConfig
  ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state
  ,lightIds :: Map IEEEAddress Int
  ,zigDevices :: Map IEEEAddress ZigDevice
  ,groups :: Map Int MQTTAPI.GroupConfig
  } deriving Show

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

hueSmallIdToLightConfig :: AppState -> Int -> MQTTAPI.LightConfig
hueSmallIdToLightConfig AppState{..} smallId = do
  case Prelude.lookup smallId (swap <$> Map.assocs lightIds) of
    Nothing -> error ("hueSmallIdToLightConfig: unknown small id:" <> show smallId)
    Just uid -> case Data.Map.lookup uid lights of
      Nothing -> error ("hueSmallIdToLightConfig: unknown uid:" <> show uid)
      Just x -> x
  

convertAction  :: HueAPI.Action -> MQTTAPI.Action
convertAction HueAPI.Action{..} = MQTTAPI.Action {
  brightness = bri
  ,color = (<$> xy) $ \case [x,y] -> ColorXY x y; _ -> error "convertAction: xy list wrong length"
  ,state = (<$> on) $ \case
     False -> OFF
     True -> ON
  ,color_temp = ct
  }

applyLightActionOnState ::  MQTTAPI.Action -> MQTTAPI.LightState -> MQTTAPI.LightState
applyLightActionOnState MQTTAPI.Action {..} =
  maybe Prelude.id (\b s -> s {state=b} :: MQTTAPI.LightState) state .
  maybe Prelude.id (\b s -> s {brightness=b} :: MQTTAPI.LightState) brightness .
  maybe Prelude.id (\b s -> s {color=Just b,color_mode=Just XYMode} :: MQTTAPI.LightState) color .
  maybe Prelude.id (\b s -> s {color_temp=Just b,color_mode=Just TemperatureMode} :: MQTTAPI.LightState) color_temp

applyLightAction :: MQTTAPI.LightConfig -> MQTTAPI.Action -> AppState -> AppState
applyLightAction MQTTAPI.LightConfig{state_topic} a st
  = st {lightStates = Map.alter (fmap (applyLightActionOnState a)) state_topic (lightStates st)}


handleLightAction :: Int -> HueAPI.Action -> AppState -> (AppState, (Text, MQTTAPI.Action))
handleLightAction hueLightId a0 st0 = (st, (t,a))
 where t = state_topic l
       a = convertAction a0
       l = hueSmallIdToLightConfig st0 hueLightId
       st = applyLightAction l a st0
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.
                                                    
lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {brightness,color_temp,state,color_mode,color}
  = HueAPI.LightState {on = state == ON
                      ,bri = brightness
                      -- ,hue = _ -- FIXME: calculate hue/sat from x/y
                      -- ,sat = _
                      ,ct = color_temp
                      ,effect = None
                      ,xy = fmap (\(ColorXY x y) -> [x,y]) color
                      ,alert = Select
                      ,colormode = (<$> color_mode) $ \case
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
          ,modelid = model -- FIXME get from topic zigbee2mqtt/bridge/devices, field model_id in the relevant element in the list. Identified by friendly_name or ieee_address
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

getLightState :: AppState -> MQTTAPI.LightConfig -> MQTTAPI.LightState
getLightState AppState{..} cfg =  Data.Map.findWithDefault blankLightState (state_topic cfg) lightStates

allHueLights :: AppState -> Map Int Light
allHueLights st@AppState{..} = Map.fromList
  [(i,lightMqtt2Hue cfg (lightStateMqtt2Hue (getLightState st cfg)))
  | (uid,cfg) <- toList lights
  , let Just i = Data.Map.lookup uid lightIds
  ]

allHueGroups :: AppState -> Map Int Group
allHueGroups st = Map.fromList [(1,group1 st)]

mkGroupWithLights :: AppState -> GroupType -> String -> Map IEEEAddress MQTTAPI.LightConfig -> Group
mkGroupWithLights st@AppState{..} _type name groupLights
  = Group {lights = [show i
                    | (uid,_) <- toList groupLights
                    , let Just i = Data.Map.lookup uid lightIds]
          ,sensors = mempty
          ,state = GroupState {all_on = and ons
                              ,any_on = or ons}
          ,recycle = False
          ,_class = Nothing
          ,action = lightStateMqtt2Hue (head (groupLightStates++[blankLightState]))
          ,..}
  where ons = [state == ON | MQTTAPI.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> toList groupLights
  
group0 :: AppState -> Group
group0 st@AppState{lights} = mkGroupWithLights st LightGroup "Group 0" lights

group1 :: AppState -> Group
group1 st@AppState{lights} = mkGroupWithLights st Room "The Void" lights

updateLightConfig :: MQTTAPI.LightConfig -> AppState -> AppState
updateLightConfig l AppState {..} =
  AppState { lights = Data.Map.insert uid l lights
           , lightIds = if uid `member` lightIds then lightIds else insert uid (1 + maximum (0 : elems lightIds)) lightIds
           , ..}
  where uid = case splitOn "_" (unique_id l) of
          (uidText:_) -> case readMaybe (unpack uidText) of
            Just x -> x
            Nothing -> error "unique_id does not have correct format (1)"
          _ -> error "unique_id does not have correct format (2)"

updateLightState :: Text -> MQTTAPI.LightState -> AppState -> AppState
updateLightState topic l AppState {..} = AppState {lightStates = Data.Map.insert topic l lightStates, ..}

