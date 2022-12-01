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

module Logic.Common
  ( AppState(..), AgendaItem(..),
    -- Device
    getDeviceByFriendlyName, topicFriendlyName,
    -- Lights
    getLightState, lightAddress, combineLightStates,
    mkLightAction,
    -- Groups
    applyGroupAction, mkGroupAction, groupScenes, groupLights,
    blankAppState,  updateLightConfig, updateLightState,
    getGroupState,
  ) where
import Data.Maybe
import MQTTAPI
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text,splitOn,unpack)
import Text.Read (readMaybe)
import Data.Aeson (ToJSON(..))
import qualified Data.List as List
import qualified Data.Text as Text

blankAppState :: AppState 
blankAppState = AppState mempty mempty mempty mempty mempty mempty mempty

data AgendaItem = forall a. ToJSON a => AgendaItem
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

lightSetTopic :: MQTTAPI.LightConfig -> Text
lightSetTopic l = state_topic l <> "/set"

lightNotifyTopic :: MQTTAPI.LightConfig -> Text
lightNotifyTopic = state_topic

groupNotifyTopic :: GroupConfig -> Text
groupNotifyTopic GroupConfig{friendly_name} = "zigbee2mqtt/" <> friendly_name

groupSetTopic :: GroupConfig -> Text
groupSetTopic g = groupNotifyTopic g  <> "/set"

mkGroupAction :: ToJSON a => GroupConfig -> a -> AgendaItem
mkGroupAction g a = AgendaItem (groupSetTopic g) a (groupNotifyTopic g)

mkLightAction :: ToJSON a => LightConfig -> a -> AgendaItem
mkLightAction l a = AgendaItem (lightSetTopic l) a (lightNotifyTopic l)
 
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


groupLights :: AppState -> GroupConfig -> [MQTTAPI.LightConfig]
groupLights AppState{lights} GroupConfig{..} =
  [l | GroupMember {ieee_address} <- members
     , Just l <- [Map.lookup ieee_address lights]]

groupScenes :: AppState -> Int -> [Int]
groupScenes AppState{groups} gid = case Map.lookup gid groups of
  Just GroupConfig {scenes} -> [ sid | SceneRef{_id=sid} <- scenes]
  Nothing -> []


getGroupState :: GroupConfig -> AppState -> Maybe LightState
getGroupState g AppState{lightStates} = Map.lookup (groupNotifyTopic g) lightStates
