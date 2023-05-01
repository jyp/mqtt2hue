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

module Logic.HueV1 (
  allHueLights, allHueGroups, getHueGroup, allHueScenes,
  translateGroupAction, translateLightAction, getCapabilities) where

import Data.Maybe
import MQTTAPI as MQTT
import HueAPI
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text,unpack,pack,isInfixOf,toCaseFold)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))
import Logic.Common
import Data.Aeson as Aeson
import Data.Aeson.KeyMap (fromList)
import qualified Data.Vector as V

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

hueSmallIdToLightConfig :: AppState -> Int -> MQTT.LightConfig
hueSmallIdToLightConfig AppState{..} smallId = do
  case Prelude.lookup smallId (swap <$> Map.assocs lightIds) of
    Nothing -> error ("hueSmallIdToLightConfig: unknown small id:" <> show smallId)
    Just uid -> case Map.lookup uid lights of
      Nothing -> error ("hueSmallIdToLightConfig: unknown uid:" <> show uid)
      Just x -> x

actionHue2Mqtt  :: HueAPI.Action -> MQTT.Action
actionHue2Mqtt HueAPI.Action{..} = MQTT.Action {
  brightness = bri
  ,color = (<$> xy) $ \case [x,y] -> ColorXY x y Nothing Nothing; _ -> error "actionHue2Mqtt: xy list wrong length"
  ,state = (<$> on) $ \case
     False -> OFF
     True -> ON
  ,color_temp = ct
  ,scene_recall = (<$> scene) $ \sid -> case readMaybe (unpack sid) of
      Nothing -> error ("scene id isn't a number: " ++ unpack sid)
      Just mqttSid -> mqttSid
  }

translateLightAction :: Int -> HueAPI.Action -> AppState -> AgendaItem
translateLightAction hueLightId a0 st0 = mkLightAction l a
 where a = actionHue2Mqtt a0
       l = hueSmallIdToLightConfig st0 hueLightId
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.

translateGroupAction  :: Int -> HueAPI.Action -> AppState -> Maybe (AppState, [AgendaItem])
translateGroupAction 0 a0 st0@AppState{groups} = do
  let a = actionHue2Mqtt a0
  -- there is no MQTT group for everything, so send a message to each group separately
  return (st0,[mkGroupAction g a | (_,g)  <- Map.assocs groups])
translateGroupAction groupId a0 st0@AppState{groups} = do
  g <- Map.lookup groupId groups
  let a = actionHue2Mqtt a0
  return (applyGroupAction st0 g a,[mkGroupAction g a])
  
            
lightStateMqtt2Hue :: MQTT.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTT.LightState {brightness,color_temp,state,color_mode,color}
  = HueAPI.LightState {on = state == ON
                      ,bri = Data.Maybe.fromMaybe 0 brightness
                      ,hue = do ColorXY {hue} <- color; hue
                      ,sat = do ColorXY {saturation} <- color; saturation
                      ,ct = color_temp
                      ,effect = None
                      ,xy = fmap (\(ColorXY {x,y}) -> [x,y]) color
                      ,alert = SelectAlert
                      ,colormode = (<$> color_mode) $ \case
                          TemperatureMode -> CT
                          XYMode -> XY
                      ,mode = HomeAutomation
                      ,reachable = True -- FIXME -- linkquality ?
                      }

lightMqtt2Hue :: TimeStamp -> MQTT.LightConfig -> HueAPI.LightState -> HueAPI.Light
lightMqtt2Hue now (MQTT.LightConfig {device = Device {name=productname,..},..}) lightState
  = Light {state = lightState 
          ,swupdate = SwUpdate {state = NoUpdates
                               ,lastinstall = now
                               }
          ,_type = if XYMode `elem` cmodes then
                     ExtendedColorLight else (if TemperatureMode `elem` cmodes
                                              then TemperatureLight
                                              else DimmableLight)
          ,name = name
          ,modelid = model -- FIXME get from topic zigbee2mqtt/bridge/devices, field model_id in the relevant element in the list. Identified by friendly_name or ieee_address
          ,manufacturername = manufacturer
          ,productname = productname
          ,capabilities = Capabilities
            {certified = False,
             control = if XYMode `elem` cmodes then
                     FullColor Other Nothing -- FIXME: Get gamut from manufacturer+modelid
                       else (if TemperatureMode `elem` cmodes
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
    where cmodes = fromMaybe [] supported_color_modes

allHueLights :: AppState -> Map Int Light
allHueLights st@AppState{..} = lightsMqtt2Hue st (Map.elems lights)

lightsMqtt2Hue :: AppState -> [MQTT.LightConfig] -> Map Int Light
lightsMqtt2Hue st@AppState{..} ls
  = Map.fromList [(i,lightMqtt2Hue appRecentTime l
                     (lightStateMqtt2Hue (getLightState st l)))
                 | l <- ls
                 , let Just i = Map.lookup (lightAddress l) lightIds]

mkGroupWithLights :: AppState -> GroupType -> Text -> Map IEEEAddress MQTT.LightConfig -> Group
mkGroupWithLights st@AppState{..} _type name ls
  = Group {lights = [pack (show i)
                    | (uid,_) <- Map.toList ls
                    , let Just i = Map.lookup uid lightIds]
          ,sensors = mempty
          ,state = GroupState {all_on = and ons
                              ,any_on = or ons}
          ,recycle = False
          ,_class = case () of
              _ | "office" `isInfixOf` nm -> Just Office
              _ | "bedroom" `isInfixOf` nm -> Just Bedroom
              _ | "garage" `isInfixOf` nm -> Just Garage
              _ | "hallway" `isInfixOf` nm -> Just Hallway
              _ | "wardrobe" `isInfixOf` nm -> Just Hallway
              _ | "kitchen" `isInfixOf` nm -> Just Kitchen
              _ | "attic" `isInfixOf` nm -> Just Attic
              _ | "living" `isInfixOf` nm -> Just LivingRoom
              _ -> Just Bedroom
          ,action = lightStateMqtt2Hue (combineLightStates groupLightStates)
          ,..}
  where ons = [state == ON | MQTT.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> Map.toList ls
        nm = toCaseFold name
  
group0 :: AppState -> MQTT.GroupConfig
group0 AppState{lights} = GroupConfig 
  {_id = 0
  ,friendly_name = "All lights"
  ,scenes = []
  ,members = [GroupMember {ieee_address = a ,endpoint = Opt1 0} | (a,_) <- Map.assocs lights]
  }
  
getHueGroup :: Int -> AppState -> Maybe (AppState,Group)
getHueGroup i st@AppState{groups} = do
  g <- case i of
    0 -> return (group0 st)
    _ -> Map.lookup i groups
  return (st,groupMqtt2Hue st g)

groupMqtt2Hue :: AppState -> GroupConfig -> Group
groupMqtt2Hue st g@GroupConfig{friendly_name}
  = mkGroupWithLights st Room friendly_name
    (Map.fromList [(lightAddress l,l) | l <- groupLights st g])
  

allHueGroups :: AppState -> Map Int Group
allHueGroups st@AppState{groups}
  = Map.fromList [ (_id,groupMqtt2Hue st g)
                 | (_id,g) <- Map.assocs groups]

allHueScenes :: AppState -> Map Int Scene
allHueScenes st@AppState{groups,appRecentTime}
  = Map.fromList
  [ (sid, -- hue bridge uses a 16 character string. But this will do.
     Scene{name = name
          ,_type = GroupScene
          ,group = Just (pack $ show gid)
          ,lights = pack . show <$> Map.keys (lightsMqtt2Hue st (groupLights st g))
          ,owner = pack $ show $ gid
          ,recycle = True
          ,locked = True
          ,appdata = Nothing
          ,picture = ""
          ,lastupdated = appRecentTime
          ,version = 2})
  | (gid,g@GroupConfig{scenes})<- Map.assocs groups
  , SceneRef{_id=sid,name} <- scenes ]



getCapabilities :: Aeson.Value
getCapabilities = Object $ fromList [
            ("lights", avail 50),
            ("sensors", avail' 60 [
                ("clip", avail 60),
                ("zll", avail 60),
                ("zgp", avail 60)]),
            ("groups", avail 60),
            ("scenes", avail' 100 [("lightstates", avail 1500)]),
            ("rules", avail' 100 [("lightstates", avail 1500)]),
            ("schedules", avail 100),
            ("resourcelinks", avail 100),
            ("whitelists", avail 100),
            ("timezones",
             Object (fromList [("value",Array (V.fromList [String "Europe/Stockholm"]))])),
            ("streaming", avail' 1  [("total",Number 10), ("channels", Number 10)])]
             
  where avail x = avail' x []
        avail' x xs = Object $ fromList (("available",Number x):xs)
