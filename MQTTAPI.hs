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

import MyAeson
import Text.Read (readMaybe)
import Data.Word (Word64)
import Numeric (showHex)

data ColorXY = ColorXY {x,y :: Float} deriving (Generic, Show, Eq)
instance FromJSON ColorXY
instance ToJSON ColorXY

newtype IEEEAddress = IEEEAddress Word64 deriving (Generic, Eq, Ord)
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


data ZigDevice = ZigDevice
  {model_id :: Maybe String -- model_id missing for controller
  ,network_address :: Int
  ,ieee_address :: IEEEAddress} deriving (Generic,Show)
instance FromJSON ZigDevice


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
  { brightness :: Int,
    color :: Maybe ColorXY,
    color_mode :: Maybe ColorMode,
    color_temp:: Maybe Int,
    linkquality:: Int,
    state :: OnOff,
    update:: UpdateState,
    update_available:: Bool
  } deriving (Generic,Show)
instance FromJSON LightState
instance ToJSON LightState

data Action = Action
  { brightness :: Maybe Int,
    color :: Maybe ColorXY,
    state :: Maybe OnOff,
    color_temp :: Maybe Int
  } deriving (Generic, Show)

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
    
data Device = Device {
            identifiers :: [Text],
            manufacturer :: Text,
            model :: Text,
            name :: Text,
            sw_version :: Text
          } deriving (Generic, Show)
instance FromJSON Device
            
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
instance FromJSON LightConfig

data GroupMember = GroupMember
  { ieee_address :: IEEEAddress,
    endpoint :: Maybe Text -- 1,2,... or left, right.
  } deriving (Show,Generic)

data SceneRef = SceneRef {_id :: Int, name :: Text}
 deriving (Show,Generic)
data GroupConfig = GroupConfig
  {     _id :: Int,
        friendly_name :: Text,
        scenes :: [SceneRef],
        members :: [GroupMember]
  } deriving (Show,Generic)
instance FromJSON GroupMember
$(myDeriveFromJSON ''SceneRef)
$(myDeriveFromJSON ''GroupConfig)
$(myDeriveToJSON ''Action)

test1 :: Maybe LightConfig
test1 = decode "{\"availability\":[{\"topic\":\"zigbee2mqtt/bridge/state\"}],\"brightness\":true,\"brightness_scale\":254,\"color_mode\":true,\"command_topic\":\"zigbee2mqtt/Led Strip TV/set\",\"device\":{\"identifiers\":[\"zigbee2mqtt_0x001788010bf4769e\"],\"manufacturer\":\"Philips\",\"model\":\"Hue white and color ambiance LightStrip plus (8718699703424)\",\"name\":\"Led Strip TV\",\"sw_version\":\"1.93.11\"},\"effect\":true,\"effect_list\":[\"blink\",\"breathe\",\"okay\",\"channel_change\",\"finish_effect\",\"stop_effect\"],\"json_attributes_topic\":\"zigbee2mqtt/Led Strip TV\",\"min_mireds\":150,\"name\":\"Led Strip TV\",\"schema\":\"json\",\"state_topic\":\"zigbee2mqtt/Led Strip TV\",\"supported_color_modes\":[\"xy\",\"color_temp\"],\"unique_id\":\"0x001788010bf4769e_light_zigbee2mqtt\"}"

-- >>> test1
-- Just (LightConfig {brightness = True, brightness_scale = Just 254, color_mode = True, command_topic = "zigbee2mqtt/Led Strip TV/set", device = Device {identifiers = ["zigbee2mqtt_0x001788010bf4769e"], manufacturer = "Philips", model = "Hue white and color ambiance LightStrip plus (8718699703424)", name = "Led Strip TV", sw_version = "1.93.11"}, effect = True, effect_list = ["blink","breathe","okay","channel_change","finish_effect","stop_effect"], json_attributes_topic = "zigbee2mqtt/Led Strip TV", max_mireds = Nothing, min_mireds = Just 150, name = "Led Strip TV", schema = "json", state_topic = "zigbee2mqtt/Led Strip TV", supported_color_modes = [XYMode,TemperatureMode], unique_id = "0x001788010bf4769e_light_zigbee2mqtt"})
