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

module Logic ( AppState(..), Todo(..),
              --  hue side
              allHueLights, allHueGroups, getHueGroup, allHueScenes,
              translateGroupAction, translateLightAction, 
              -- MQTT side
              blankAppState,  updateLightConfig, updateLightState,
              -- Automations
              handleSwitchState
             ) where
import Data.Maybe
import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text,splitOn,unpack,pack,isInfixOf,toCaseFold)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))
import Data.Aeson (ToJSON(..))
import qualified Data.List as List
import qualified Data.Text as Text
import Control.Monad

blankAppState :: AppState 
blankAppState = AppState mempty mempty mempty mempty mempty mempty mempty

data Todo = forall a. ToJSON a => Todo
  { outTopic :: Text
  , message :: a
  , notifyTopic :: Text }

data AppState = AppState
  {lights :: Map IEEEAddress MQTTAPI.LightConfig
  ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state. Lights and groups here.
  ,switchStates :: Map Text MQTTAPI.SwitchState -- map from topic to state. Switches here.
  ,lightIds :: Map IEEEAddress Int -- give hue v1 id here
  ,zigDevices :: Map IEEEAddress ZigDevice
  ,groups :: Map Int MQTTAPI.GroupConfig
  ,groupLastRecallScene :: Map Int Int -- map from group id to last activated scene
  } deriving Show

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

hueSmallIdToLightConfig :: AppState -> Int -> MQTTAPI.LightConfig
hueSmallIdToLightConfig AppState{..} smallId = do
  case Prelude.lookup smallId (swap <$> Map.assocs lightIds) of
    Nothing -> error ("hueSmallIdToLightConfig: unknown small id:" <> show smallId)
    Just uid -> case Map.lookup uid lights of
      Nothing -> error ("hueSmallIdToLightConfig: unknown uid:" <> show uid)
      Just x -> x

actionHue2Mqtt  :: HueAPI.Action -> MQTTAPI.Action
actionHue2Mqtt HueAPI.Action{..} = MQTTAPI.Action {
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

translateLightAction :: Int -> HueAPI.Action -> AppState -> Todo
translateLightAction hueLightId a0 st0 = Todo (lightSetTopic l) a (lightNotifyTopic l)
 where a = actionHue2Mqtt a0
       l = hueSmallIdToLightConfig st0 hueLightId
       -- update the state so that immediate queries will get the
       -- optimistically updated state. The real state update will
       -- occur later when MQTT sends back the true updated light state.

lightSetTopic :: MQTTAPI.LightConfig -> Text
lightSetTopic l = state_topic l <> "/set"

lightNotifyTopic :: MQTTAPI.LightConfig -> Text
lightNotifyTopic = state_topic

groupNotifyTopic :: GroupConfig -> Text
groupNotifyTopic GroupConfig{friendly_name} = "zigbee2mqtt/" <> friendly_name

groupSetTopic :: GroupConfig -> Text
groupSetTopic g = groupNotifyTopic g  <> "/set"

mkGroupAction :: ToJSON a => GroupConfig -> a -> Todo
mkGroupAction g a = Todo (groupSetTopic g) a (groupNotifyTopic g)

translateGroupAction  :: Int -> HueAPI.Action -> AppState -> Maybe (AppState, [Todo])
translateGroupAction 0 a0 st0@AppState{groups} = do
  let a = actionHue2Mqtt a0
  -- there is no MQTT group for everything, so send a message to each group separately
  return (st0,[mkGroupAction g a | (_,g)  <- Map.assocs groups])
translateGroupAction groupId a0 st0@AppState{groups} = do
  g <- Map.lookup groupId groups
  let a = actionHue2Mqtt a0
  return (applyGroupAction st0 g a,[mkGroupAction g a])

applyGroupAction :: AppState -> GroupConfig -> MQTTAPI.Action -> AppState
applyGroupAction st0@AppState {groupLastRecallScene}
  GroupConfig {_id=gid}
  (MQTTAPI.Action {scene_recall = Just sid}) =
  st0 {groupLastRecallScene = Map.insert gid sid groupLastRecallScene}
applyGroupAction st _ _ = st
  

getDeviceByFriendlyName :: Text -> AppState -> Maybe ZigDevice
getDeviceByFriendlyName nm AppState{zigDevices} =
  List.find (\ZigDevice{friendly_name=n} -> n == nm) (Map.elems zigDevices)

topicFriendlyName :: Text -> Text
topicFriendlyName = Text.tail . Text.dropWhile (/= '/')

-- >>> topicFriendlyName (Text.pack "zigbee2mqtt/Nice Stuff")
-- "Nice Stuff"
  
handleSwitchState :: Text -> SwitchState -> AppState -> (AppState, [Todo])
handleSwitchState t SwitchState { action = BtnAction OnBtn Press }
                    st0@AppState{groups,lightStates,groupLastRecallScene}
  = fromMaybe (st0,[]) $ do
      dev <- getDeviceByFriendlyName (topicFriendlyName t) st0
      let gids = [ gid | ep <- Map.elems (endpoints dev)
                       , Binding cluster
                         (Target {_id = Just gid,_type = "group"}) <- bindings ep
                       , cluster == "genOnOff"]
      gid <- listToMaybe gids
      g <- Map.lookup gid groups
      MQTTAPI.LightState {state} <- Map.lookup (groupNotifyTopic g) lightStates
      -- return [Todo "hue2mqtt/debug/switchscene/group" g "",
      --         Todo "hue2mqtt/debug/switchscene/state" state ""]
      when (state  == OFF) $
        fail "don't change scene if group is off"
      let lastScene = Map.findWithDefault lastAvailableScene gid groupLastRecallScene
          gss = groupScenes st0 gid
          -- by default take the last scene in the list, so the activated scene will be the 1st
          lastAvailableScene:_ = reverse gss ++ [0]
      -- return [Todo "hue2mqtt/debug/switchscene/lastscene" lastScene ""
      --        ,Todo "hue2mqtt/debug/switchscene/lastAvailableScene" lastAvailableScene ""]
      case dropWhile (/= lastScene) (gss ++ gss) of -- copy in case its last
        (_:nextScene:_) -> do
          let a = blankAction {scene_recall = Just nextScene}
          return (applyGroupAction st0 g a, [mkGroupAction g a])
        _ -> fail "next scene not found"
handleSwitchState _ _ st = (st, [])

            
lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {brightness,color_temp,state,color_mode,color}
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
    (Map.findWithDefault blankLightState (state_topic cfg) lightStates)

-- | Invent some colors if the light supports them but we did not get
-- them from the state yet. (otherwise Gnome app won't see that it
-- does in fact support them) (???)
enrichLightState :: [MQTTAPI.ColorMode] -> MQTTAPI.LightState -> MQTTAPI.LightState
enrichLightState cmodes ls@MQTTAPI.LightState{color,color_temp} =
  ls {color =
         if XYMode `elem` cmodes && isNothing color
         then Just (ColorXY 0.4 0.4 Nothing Nothing)
         else color
     ,color_temp =
         if TemperatureMode `elem` cmodes && isNothing color_temp
         then Just 400
         else color_temp}

allHueLights :: AppState -> Map Int Light
allHueLights st@AppState{..} = lightsMqtt2Hue st (Map.elems lights)

lightsMqtt2Hue :: AppState -> [MQTTAPI.LightConfig] -> Map Int Light
lightsMqtt2Hue st@AppState{..} ls
  = Map.fromList [(i,lightMqtt2Hue l (lightStateMqtt2Hue (getLightState st l)))
                 | l <- ls
                 , let Just i = Map.lookup (lightAddress l) lightIds]

class Avg a where
  (+.) :: a -> a -> a
  divide :: a -> Int -> a
instance Avg Int where
  (+.) = (+)
  divide = div
instance Avg ColorXY where
  ColorXY x1 y1 _ _ +. ColorXY x2 y2 _ _ = ColorXY (x1+x2) (y1+y2) Nothing Nothing
  divide (ColorXY x y _ _) n = ColorXY (x / fromIntegral n) (y / fromIntegral n) Nothing Nothing
  
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
  where ons = [state == ON | MQTTAPI.LightState{state} <- groupLightStates  ]
        groupLightStates = getLightState st . snd <$> Map.toList ls
        nm = toCaseFold name
  
group0 :: AppState -> MQTTAPI.GroupConfig
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

lightAddress :: MQTTAPI.LightConfig -> IEEEAddress
lightAddress l = uid
  where uid = case splitOn "_" (unique_id l) of
          (uidText:_) -> case readMaybe (unpack uidText) of
            Just x -> x
            Nothing -> error "unique_id does not have correct format (1)"
          _ -> error "unique_id does not have correct format (2)"

updateLightConfig :: MQTTAPI.LightConfig -> AppState -> AppState
updateLightConfig l AppState {..} =
  AppState { lights = Map.insert uid l lights
           , lightIds = if uid `Map.member` lightIds then lightIds else Map.insert uid (1 + maximum (0 : Map.elems lightIds)) lightIds
           , ..}
  where uid = lightAddress l

updateLightState :: Text -> MQTTAPI.LightState -> AppState -> AppState
updateLightState topic l AppState {..} =
  AppState {lightStates = Map.insert topic l lightStates
           ,..}

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
          ,lights = pack . show <$> Map.keys (lightsMqtt2Hue st (groupLights st g))
          ,owner = pack $ show $ gid
          ,recycle = True
          ,locked = True
          ,appdata = Nothing
          ,picture = ""
          ,lastupdated = zeroTime
          ,version = 2})
  | (gid,g@GroupConfig{scenes})<- Map.assocs groups
  , SceneRef{_id=sid,name} <- scenes ]

groupScenes :: AppState -> Int -> [Int]
groupScenes AppState{groups} gid = case Map.lookup gid groups of
  Just GroupConfig {scenes} -> [ sid | SceneRef{_id=sid} <- scenes]
  Nothing -> []
