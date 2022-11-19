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
              allHueLights, allHueGroups, handleLightAction, handleGroupAction, getHueGroup,
              -- MQTT side
              blankAppState,  updateLightConfig, updateLightState, 
             ) where

import Data.Maybe
import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map
import Data.Text (Text,splitOn,unpack,pack)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))

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
  maybe id (\b s -> s {state=b} :: MQTTAPI.LightState) state .
  maybe id (\b s -> s {brightness=Just b} :: MQTTAPI.LightState) brightness .
  maybe id (\b s -> s {color=Just b,color_mode=Just XYMode} :: MQTTAPI.LightState) color .
  maybe id (\b s -> s {color_temp=Just b,color_mode=Just TemperatureMode} :: MQTTAPI.LightState) color_temp

applyLightAction :: MQTTAPI.Action -> MQTTAPI.LightConfig -> AppState -> AppState
applyLightAction a MQTTAPI.LightConfig{state_topic} st
  = st {lightStates = Map.alter (fmap (applyLightActionOnState a)) state_topic (lightStates st)}

applyGroupAction :: MQTTAPI.Action -> MQTTAPI.GroupConfig -> AppState -> AppState
applyGroupAction a g st = Prelude.foldr (applyLightAction a) st (groupLights st g)

handleLightAction :: Int -> HueAPI.Action -> AppState -> (AppState, (Text, MQTTAPI.Action))
handleLightAction hueLightId a0 st0 = (st, (t,a)) 
 where t = state_topic l <> "/set"
       a = convertAction a0
       l = hueSmallIdToLightConfig st0 hueLightId
       st = applyLightAction a l st0
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.

handleGroupAction :: Int -> HueAPI.Action -> AppState -> Maybe (AppState, (Text, MQTTAPI.Action))
handleGroupAction groupId a0 st0@AppState{groups} = do
  g@GroupConfig{friendly_name} <- if groupId == 0 then Just (group0 st0) else Data.Map.lookup groupId groups
  let t = "zigbee2mqtt/" <> friendly_name  <> "/set"
      a = convertAction a0
      st = applyGroupAction a g st0
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.
  return (st, (t,a))

            
lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {brightness,color_temp,state,color_mode,color}
  = HueAPI.LightState {on = state == ON
                      ,bri = Data.Maybe.fromMaybe 0 brightness
                      -- ,hue = _ -- FIXME: calculate hue/sat from x/y
                      -- ,sat = _
                      ,ct = color_temp
                      ,effect = None
                      ,xy = fmap (\(ColorXY x y) -> [x,y]) color
                      ,alert = SelectAlert
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

blankLightState :: MQTTAPI.LightState
blankLightState = MQTTAPI.LightState
  {brightness = Nothing
  ,color = Nothing
  ,color_mode = Nothing
  ,color_temp = Nothing
  ,linkquality = 0
  ,state = OFF
  ,update = UpdateState {state = MQTTAPI.Idle}
  ,update_available = False
  }

getLightState :: AppState -> MQTTAPI.LightConfig -> MQTTAPI.LightState
getLightState AppState{..} cfg
  = enrichLightState (supported_color_modes cfg)
    (Data.Map.findWithDefault blankLightState (state_topic cfg) lightStates)

-- | Invent some colors if the light support them (otherwise Gnome app
-- won't see it does in fact support them)
enrichLightState :: [MQTTAPI.ColorMode] -> MQTTAPI.LightState -> MQTTAPI.LightState
enrichLightState cmodes ls@MQTTAPI.LightState{color,color_temp} =
  ls {color =
         if XYMode `elem` cmodes && isNothing color
         then Just (ColorXY 0.4 0.4)
         else color
     ,color_temp =
         if TemperatureMode `elem` cmodes && isNothing color_temp
         then Just 400
         else color_temp}

allHueLights :: AppState -> Map Int Light
allHueLights st@AppState{..} = Map.fromList
  [(i,lightMqtt2Hue cfg (lightStateMqtt2Hue (getLightState st cfg)))
  | (uid,cfg) <- toList lights
  , let Just i = Data.Map.lookup uid lightIds
  ]

mkGroupWithLights :: AppState -> GroupType -> Text -> Map IEEEAddress MQTTAPI.LightConfig -> Group
mkGroupWithLights st@AppState{..} _type name ls
  = Group {lights = [pack (show i)
                    | (uid,_) <- toList ls
                    , let Just i = Data.Map.lookup uid lightIds]
          ,sensors = mempty
          ,state = GroupState {all_on = and ons
                              ,any_on = or ons}
          ,recycle = False
          ,_class = Nothing
          ,action = lightStateMqtt2Hue (head (groupLightStates++[blankLightState])) -- FIXME
          ,..}
  where ons = [state == ON | MQTTAPI.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> toList ls
  
group0 :: AppState -> MQTTAPI.GroupConfig
group0 AppState{lights} = GroupConfig 
  {_id = 0
  ,friendly_name = "All lights"
  ,scenes = []
  ,members = [GroupMember {ieee_address = a ,endpoint = Opt1 0} | (a,_) <- assocs lights]
  }
  
getHueGroup :: Int -> AppState -> Maybe (AppState,Group)
getHueGroup i st@AppState{groups} = do
  g <- case i of
    0 -> return (group0 st)
    _ -> Data.Map.lookup i groups
  return (st,groupMqtt2Hue st g)

lightAddress :: MQTTAPI.LightConfig -> IEEEAddress
lightAddress l = uid
  where uid = case splitOn "_" (unique_id l) of
          (uidText:_) -> case readMaybe (unpack uidText) of
            Just x -> x
            Nothing -> error "unique_id does not have correct format (1)"
          _ -> error "unique_id does not have correct format (2)"

updateLightConfig :: MQTTAPI.LightConfig -> AppState -> AppState
updateLightConfig l AppState {..} =
  AppState { lights = Data.Map.insert uid l lights
           , lightIds = if uid `member` lightIds then lightIds else insert uid (1 + maximum (0 : elems lightIds)) lightIds
           , ..}
  where uid = lightAddress l

updateLightState :: Text -> MQTTAPI.LightState -> AppState -> AppState
updateLightState topic l AppState {..} = AppState {lightStates = Data.Map.insert topic l lightStates, ..}


groupMqtt2Hue :: AppState -> GroupConfig -> Group
groupMqtt2Hue st g@GroupConfig{friendly_name}
  = mkGroupWithLights st Room friendly_name
      (Map.fromList [(lightAddress l,l) | l <- groupLights st g])

groupLights :: AppState -> GroupConfig -> [MQTTAPI.LightConfig]
groupLights AppState{lights} GroupConfig{..} =
  [l | GroupMember {ieee_address} <- members
     , Just l <- [Map.lookup ieee_address lights]]
  

allHueGroups :: AppState -> Map Int Group
allHueGroups st@AppState{groups}
  = Map.fromList [ (_id,groupMqtt2Hue st g)
                 | (_id,g) <- Map.assocs groups]
