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
              allHueLights, allHueGroups, handleLightAction, handleGroupAction, getHueGroup, allHueScenes,
              -- MQTT side
              blankAppState,  updateLightConfig, updateLightState, 
             ) where
import Data.Maybe
import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map
import Data.Text (Text,splitOn,unpack,pack,isInfixOf,toCaseFold)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))

blankAppState :: AppState 
blankAppState = AppState mempty mempty mempty mempty mempty

data AppState = AppState
  {lights :: Map IEEEAddress MQTTAPI.LightConfig
  ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state. Lights and groups here.
  ,lightIds :: Map IEEEAddress Int -- give hue v1 id here
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
  

actionHue2Mqtt  :: HueAPI.Action -> MQTTAPI.Action
actionHue2Mqtt HueAPI.Action{..} = MQTTAPI.Action {
  brightness = bri
  ,color = (<$> xy) $ \case [x,y] -> ColorXY x y; _ -> error "actionHue2Mqtt: xy list wrong length"
  ,state = (<$> on) $ \case
     False -> OFF
     True -> ON
  ,color_temp = ct
  ,scene_recall = (<$> scene) $ \sid -> case readMaybe (unpack sid) of
      Nothing -> error ("scene id isn't a number: " ++ unpack sid)
      Just mqttSid -> mqttSid
  }

applyLightActionOnState ::  [MQTTAPI.ColorMode] -> MQTTAPI.Action -> MQTTAPI.LightState -> MQTTAPI.LightState
applyLightActionOnState supported MQTTAPI.Action {..} =
                         maybe id (\b s -> s {state=b} :: MQTTAPI.LightState) state .
                         maybe id (\b s -> s {brightness=Just b} :: MQTTAPI.LightState) brightness .
  cMode XYMode          (maybe id (\b s -> s {color=Just b,color_mode=Just XYMode} :: MQTTAPI.LightState) color) .
  cMode TemperatureMode (maybe id (\b s -> s {color_temp=Just b,color_mode=Just TemperatureMode} :: MQTTAPI.LightState) color_temp)
  where cMode m f = if m `elem` supported then f else id

applyLightAction :: MQTTAPI.Action -> MQTTAPI.LightConfig -> AppState -> AppState
applyLightAction a MQTTAPI.LightConfig{state_topic,supported_color_modes} st
  = st {lightStates = Map.alter (fmap (applyLightActionOnState supported_color_modes a)) state_topic (lightStates st)}

applyGroupAction :: MQTTAPI.Action -> MQTTAPI.GroupConfig -> AppState -> AppState
applyGroupAction a g st = Prelude.foldr (applyLightAction a) st (groupLights st g)

handleLightAction :: Int -> HueAPI.Action -> AppState -> (AppState, (Text, MQTTAPI.Action))
handleLightAction hueLightId a0 st0 = (st, (t,a)) 
 where t = lightSetTopic l
       a = actionHue2Mqtt a0
       l = hueSmallIdToLightConfig st0 hueLightId
       st = applyLightAction a l st0
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.

lightSetTopic :: MQTTAPI.LightConfig -> Text
lightSetTopic l = state_topic l <> "/set"

groupSetTopic :: GroupConfig -> Text
groupSetTopic GroupConfig{friendly_name} = "zigbee2mqtt/" <> friendly_name  <> "/set"

handleGroupAction :: Int -> HueAPI.Action -> AppState -> Maybe (AppState, [(Text, MQTTAPI.Action)])
handleGroupAction 0 a0 st0@AppState{groups} = do
  let g = group0 st0
  let a = actionHue2Mqtt a0
      st = applyGroupAction a g st0
  -- there is no MQTT group for everything, so send a message to each group individually
  return (st, [(groupSetTopic l,a) | (_,l)  <- assocs groups])
handleGroupAction groupId a0 st0@AppState{groups} = do
  g <- Data.Map.lookup groupId groups
  let t = groupSetTopic g
      a = actionHue2Mqtt a0
      st = applyGroupAction a g st0
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.
  return (st, [(t,a)])

            
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

zeroTime :: UTCTime
zeroTime = UTCTime (toEnum 0) (toEnum 0)

lightMqtt2Hue :: MQTTAPI.LightConfig -> HueAPI.LightState -> HueAPI.Light
lightMqtt2Hue (MQTTAPI.LightConfig {device = Device {name=productname,..},..}) lightState
  = Light {state = lightState 
          ,swupdate = SwUpdate {state = NoUpdates
                               ,lastinstall = zeroTime -- FIXME
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
  ,linkquality = Nothing
  ,state = OFF
  ,update = Nothing
  ,update_available = Nothing
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
allHueLights st@AppState{..} = lightsMqtt2Hue st (elems lights)

lightsMqtt2Hue :: AppState -> [MQTTAPI.LightConfig] -> Map Int Light
lightsMqtt2Hue st@AppState{..} ls
  = Map.fromList [(i,lightMqtt2Hue l (lightStateMqtt2Hue (getLightState st l)))
                 | l <- ls
                 , let Just i = Data.Map.lookup (lightAddress l) lightIds]

class Avg a where
  (+.) :: a -> a -> a
  divide :: a -> Int -> a
instance Avg Int where
  (+.) = (+)
  divide = div
instance Avg ColorXY where
  ColorXY x1 y1 +. ColorXY x2 y2 = ColorXY (x1+x2) (y1+y2)
  divide (ColorXY x y) n = ColorXY (x / fromIntegral n) (y / fromIntegral n)
  
average :: Avg a => [a] -> Maybe a
average [] = Nothing
average xs = Just (Prelude.foldr1 (+.) xs `divide` length xs)

orState :: OnOff -> OnOff -> OnOff
orState ON _ = ON
orState OFF x = x

combineLightStates :: [MQTTAPI.LightState] -> MQTTAPI.LightState
combineLightStates ls = MQTTAPI.LightState
  {brightness = average $ catMaybes [brightness | MQTTAPI.LightState{brightness} <- ls]
  ,color = average $ catMaybes [color | MQTTAPI.LightState{color} <- ls]
  ,color_mode = case catMaybes [color_mode | MQTTAPI.LightState{color_mode} <- ls] of
      [] -> Nothing
      xs -> Just  $ if all (== XYMode) xs
                    then XYMode
                    else TemperatureMode
  ,color_temp = average $ catMaybes [color_temp | MQTTAPI.LightState{color_temp} <- ls]
  ,linkquality = Nothing
  ,state = Prelude.foldr orState OFF ([state | MQTTAPI.LightState{state} <- ls])
  ,update = Nothing
  ,update_available = Nothing
  }


mkGroupWithLights :: AppState -> GroupType -> Text -> Map IEEEAddress MQTTAPI.LightConfig -> Group
mkGroupWithLights st@AppState{..} _type name ls
  = Group {lights = [pack (show i)
                    | (uid,_) <- toList ls
                    , let Just i = Data.Map.lookup uid lightIds]
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
  where ons = [state == ON | MQTTAPI.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> toList ls
        nm = toCaseFold name
  
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

allHueScenes :: AppState -> Map Int Scene
allHueScenes st@AppState{groups}
  = Map.fromList
  [ (sid, -- hue bridge uses a 16 character string. But this will do.
     Scene{name = name
          ,_type = GroupScene
          ,group = Just (pack $ show gid)
          ,lights = pack . show <$> Data.Map.keys (lightsMqtt2Hue st (groupLights st g))
          ,owner = pack $ show $ gid
          ,recycle = True
          ,locked = True
          ,appdata = Nothing
          ,picture = ""
          ,lastupdated = zeroTime
          ,version = 2})
  | (gid,g@GroupConfig{scenes})<- assocs groups
  , SceneRef{_id=sid,name} <- scenes ]
