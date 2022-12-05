{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MQTTAPI where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Map

import MyAeson
import Text.Read (readMaybe)
import Data.Word (Word64)
import Numeric (showHex)
import Types

data ColorXY = ColorXY {x,y :: Float, hue, saturation :: Maybe Int} deriving (Generic, Show, Eq)
instance FromJSON ColorXY
instance ToJSON ColorXY

newtype IEEEAddress = IEEEAddress Word64 deriving (Generic, Eq, Ord)
instance LargeHashable IEEEAddress
instance Show IEEEAddress where
  showsPrec _ (IEEEAddress x) = showString ("0x"  <> pad (showHex x []))
    where pad = Prelude.reverse . Prelude.take 16 .  (++ repeat '0') . Prelude.reverse
instance Read IEEEAddress where
  readsPrec d s = [(IEEEAddress x,r) | (x,r) <- readsPrec d s]

-- >>> readMaybe "0x001788010bf4769e" :: Maybe IEEEAddress
-- Just 0x001788010bf4769e

instance FromJSON IEEEAddress where
  parseJSON = \case
    String s -> case readMaybe (unpack s) of
      Nothing ->  fail "fromJSON: invalid IEEE address number"
      Just x -> return (IEEEAddress x)
    _ -> fail "fromJSON: invalid IEEE address"
instance ToJSON IEEEAddress where
  toJSON x = String (pack (show x))

data ZigDevice = ZigDevice
  {model_id :: Maybe Text -- model_id missing for controller
  ,network_address :: Int
  ,ieee_address :: IEEEAddress
  ,endpoints :: Map Int Endpoint
  ,friendly_name :: Text
  } deriving (Generic,Show)

data ColorMode = TemperatureMode | XYMode deriving (Generic, Show, Eq)
instance FromJSON ColorMode where
  parseJSON = \case
    String "color_temp" -> return TemperatureMode
    String "xy" -> return XYMode
    _ -> fail "fromJSON: mqtt: invalid color mode"
  
instance ToJSON ColorMode where
  toJSON = \case
    TemperatureMode -> "color_temp"
    XYMode -> "xy"

data OnOff = OFF | ON deriving (Generic, Eq, Show)
instance FromJSON OnOff
instance ToJSON OnOff

data UpdateState = UpdateState {state :: Status} deriving (Generic,Show)
instance FromJSON UpdateState
instance ToJSON UpdateState
data LightState = LightState
  { brightness :: Maybe Int,
    color :: Maybe ColorXY,
    color_mode :: Maybe ColorMode,
    color_temp :: Maybe Int,
    linkquality :: Maybe Int,
    state :: OnOff,
    update:: Maybe UpdateState,
    update_available:: Maybe Bool
  } deriving (Generic,Show)
instance FromJSON LightState
instance ToJSON LightState

-- >>> decodeTestFile "examples/MQTT/ls0.json" :: IO (Maybe LightState)
-- Just (LightState {brightness = Just 243, color = Just (ColorXY {x = 0.3612283, y = 0.113979965}), color_mode = Just XYMode, color_temp = Just 153, linkquality = Nothing, state = ON, update = Nothing, update_available = Nothing})

data BtnType = OnBtn | OffBtn | OtherBtn Text deriving Show

data BtnState = Press | Release | Hold deriving Show

data ActionType = NoAction | BtnAction BtnType BtnState deriving Show

parseBtn :: Text -> BtnType
parseBtn = \case
  "on" -> OnBtn
  "off" -> OffBtn
  x -> OtherBtn x

parseState = \case
  "press" -> pure Press
  "release" -> pure Release
  "hold" -> pure Hold
  _ -> fail "fromJSON: mqtt: invalid action state" 

instance FromJSON ActionType where
  parseJSON = \case
    String "" -> return NoAction
    String s -> case splitOn "-" s of
                 [btn,state] -> BtnAction (parseBtn btn) <$> parseState state
                 _ -> fail "fromJSON: mqtt: invalid action format"
    _ -> fail "fromJSON: mqtt: invalid action type" 

data SwitchState = SwitchState { action :: ActionType } deriving (Show,Generic)

instance FromJSON SwitchState
  
data Action = Action
  { brightness :: Maybe Int,
    color :: Maybe ColorXY,
    state :: Maybe OnOff,
    color_temp :: Maybe Int,
    scene_recall :: Maybe Int
  } deriving (Generic, Show)

blankAction :: Action
blankAction = Action Nothing Nothing Nothing Nothing Nothing 
data Status = Idle | Busy
  deriving (Eq, Show, Generic)
instance FromJSON Status where
  parseJSON = \case
    String "idle" -> return Idle
    _ -> return Busy
instance ToJSON Status where
  toJSON = \case
    Idle -> "idle"
    Busy -> "busy"

data Target = Target
  { _id :: Maybe Int
  , _type :: Text
  } deriving (Generic, Show)

data Binding = Binding
  { cluster :: Text
  , target :: Target
  } deriving (Generic, Show)



data Endpoint = Endpoint
  { bindings :: [Binding]
  -- , clusters :: Map Text [Text]
  } deriving (Generic, Show)
    
data Device = Device {
            identifiers :: [Text],
            manufacturer :: Text,
            model :: Text,
            name :: Text,
            sw_version :: Text
          } deriving (Generic, Show)


         
data LightConfig = LightConfig {
          brightness :: Bool,
          brightness_scale :: Maybe Int,
          color_mode :: Bool,
          command_topic :: Text,
          device :: Device,
          effect :: Bool,
          effect_list :: [Text],
          json_attributes_topic :: Text,
          max_mireds :: Maybe Int,
          min_mireds :: Maybe Int,
          name :: Text,
          -- schema :: Text, -- always json
          state_topic :: Text,
          supported_color_modes :: [ColorMode],
          unique_id :: Text
        } deriving (Generic, Show)
  

data GroupMember = GroupMember
  { ieee_address :: IEEEAddress
  , endpoint :: Choice Int Text -- 1,2,... or left, right.
  } deriving (Show,Generic)

data SceneRef = SceneRef {_id :: Int, name :: Text}
 deriving (Show,Generic)
data GroupConfig = GroupConfig
  {_id :: Int
  ,friendly_name :: Text
  ,scenes :: [SceneRef]
  ,members :: [GroupMember]
  } deriving (Show,Generic)
instance FromJSON GroupMember
instance ToJSON GroupMember
$(myDeriveJSON ''SceneRef)
$(myDeriveJSON ''GroupConfig)
$(myDeriveFromJSON ''Action)
$(myDeriveToJSON ''Action)
$(myDeriveFromJSON ''Target)
$(myDeriveToJSON ''Target)
instance FromJSON Binding
instance ToJSON Binding
instance FromJSON Endpoint
instance ToJSON Endpoint
instance FromJSON ZigDevice
instance ToJSON ZigDevice
instance FromJSON Device
instance FromJSON LightConfig

test0 :: Maybe [GroupConfig]

test0 = decode "[{\"friendly_name\":\"Living Room Upstairs\",\"id\":1,\"members\":[{\"endpoint\":11,\"ieee_address\":\"0x001788010bf4769e\"}],\"scenes\":[]}]"

-- >>> test0
-- Just [GroupConfig {_id = 1, friendly_name = "Living Room Upstairs", scenes = [], members = [GroupMember {ieee_address = 0x001788010bf4769e, endpoint = Opt1 11}]}]

test1 :: Maybe LightConfig
test1 = decode "{\"availability\":[{\"topic\":\"zigbee2mqtt/bridge/state\"}],\"brightness\":true,\"brightness_scale\":254,\"color_mode\":true,\"command_topic\":\"zigbee2mqtt/Led Strip TV/set\",\"device\":{\"identifiers\":[\"zigbee2mqtt_0x001788010bf4769e\"],\"manufacturer\":\"Philips\",\"model\":\"Hue white and color ambiance LightStrip plus (8718699703424)\",\"name\":\"Led Strip TV\",\"sw_version\":\"1.93.11\"},\"effect\":true,\"effect_list\":[\"blink\",\"breathe\",\"okay\",\"channel_change\",\"finish_effect\",\"stop_effect\"],\"json_attributes_topic\":\"zigbee2mqtt/Led Strip TV\",\"min_mireds\":150,\"name\":\"Led Strip TV\",\"schema\":\"json\",\"state_topic\":\"zigbee2mqtt/Led Strip TV\",\"supported_color_modes\":[\"xy\",\"color_temp\"],\"unique_id\":\"0x001788010bf4769e_light_zigbee2mqtt\"}"

-- >>> test1
-- Just (LightConfig {brightness = True, brightness_scale = Just 254, color_mode = True, command_topic = "zigbee2mqtt/Led Strip TV/set", device = Device {manufacturer = "Philips"}, effect = True, effect_list = ["blink","breathe","okay","channel_change","finish_effect","stop_effect"], json_attributes_topic = "zigbee2mqtt/Led Strip TV", max_mireds = Nothing, min_mireds = Just 150, name = "Led Strip TV", state_topic = "zigbee2mqtt/Led Strip TV", supported_color_modes = [XYMode,TemperatureMode], unique_id = "0x001788010bf4769e_light_zigbee2mqtt"})

-- >>> decodeTestFile "examples/MQTT/devices.json" :: IO (Maybe [ZigDevice])
-- Just [ZigDevice {model_id = Nothing, network_address = 0, ieee_address = 0x00124b0025e1df38, endpoints = fromList [(1,Endpoint {bindings = []}),(2,Endpoint {bindings = []}),(3,Endpoint {bindings = []}),(4,Endpoint {bindings = []}),(5,Endpoint {bindings = []}),(6,Endpoint {bindings = []}),(8,Endpoint {bindings = []}),(10,Endpoint {bindings = []}),(11,Endpoint {bindings = []}),(12,Endpoint {bindings = []}),(13,Endpoint {bindings = []}),(47,Endpoint {bindings = []}),(110,Endpoint {bindings = []}),(242,Endpoint {bindings = []})]},ZigDevice {model_id = Just "LCL001", network_address = 31546, ieee_address = 0x001788010bf4769e, endpoints = fromList [(11,Endpoint {bindings = [Binding {cluster = "genOnOff", target = Target {_id = Nothing, _type = "endpoint"}},Binding {cluster = "genLevelCtrl", target = Target {_id = Nothing, _type = "endpoint"}}]}),(242,Endpoint {bindings = []})]},ZigDevice {model_id = Just "lumi.airrtc.agl001", network_address = 41905, ieee_address = 0x54ef44100057be39, endpoints = fromList [(1,Endpoint {bindings = []})]},ZigDevice {model_id = Just "RWL021", network_address = 15863, ieee_address = 0x0017880106e804ef, endpoints = fromList [(1,Endpoint {bindings = [Binding {cluster = "genScenes", target = Target {_id = Just 1, _type = "group"}},Binding {cluster = "genOnOff", target = Target {_id = Just 1, _type = "group"}},Binding {cluster = "genLevelCtrl", target = Target {_id = Just 1, _type = "group"}}]}),(2,Endpoint {bindings = [Binding {cluster = "manuSpecificUbisysDeviceSetup", target = Target {_id = Nothing, _type = "endpoint"}},Binding {cluster = "genPowerCfg", target = Target {_id = Nothing, _type = "endpoint"}}]})]}]

-- >>> decodeTestFile "examples/MQTT/btnState.json" :: IO (Maybe SwitchState)
-- Just (SwitchState {action = BtnAction OnBtn Press})
