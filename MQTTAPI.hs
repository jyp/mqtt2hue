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
{-# LANGUAGE DisambiguateRecordFields #-}


import GHC.Generics
import qualified Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString

data ColorXY = ColorXY {x,y :: Float} deriving (Generic, Show, Eq)
instance FromJSON ColorXY
instance ToJSON ColorXY

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

data OnOff = OFF | ON deriving Generic
instance FromJSON OnOff
instance ToJSON OnOff

data UpdateState = UpdateState {state :: Status} deriving Generic
instance FromJSON UpdateState
instance ToJSON UpdateState
data LightState = LightState
  { brightness :: Int,
    color :: ColorXY,
    color_mode :: ColorMode,
    color_temp:: Int,
    linkquality:: Int,
    state:: OnOff,
    update:: UpdateState,
    update_available:: Bool
  } deriving Generic
instance FromJSON LightState
instance ToJSON LightState

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
            identifiers :: [String],
            manufacturer :: String,
            model :: String,
            name :: String,
            sw_version :: String
          } deriving (Generic, Show)
instance FromJSON Device
            
data LightConfig = LightConfig {
          brightness :: Bool,
          brightness_scale :: Maybe Int,
          color_mode :: Bool,
          command_topic :: String,
          device :: Device,
          effect :: Bool,
          effect_list :: [String],
          json_attributes_topic :: String,
          max_mireds :: Maybe Int,
          min_mireds :: Maybe Int,
          name :: String,
          schema :: String,
          state_topic :: String,
          supported_color_modes :: [ColorMode],
          unique_id :: String
        } deriving (Generic, Show)
instance FromJSON LightConfig

test1 :: Maybe LightConfig
test1 = decode "{\"availability\":[{\"topic\":\"zigbee2mqtt/bridge/state\"}],\"brightness\":true,\"brightness_scale\":254,\"color_mode\":true,\"command_topic\":\"zigbee2mqtt/Led Strip TV/set\",\"device\":{\"identifiers\":[\"zigbee2mqtt_0x001788010bf4769e\"],\"manufacturer\":\"Philips\",\"model\":\"Hue white and color ambiance LightStrip plus (8718699703424)\",\"name\":\"Led Strip TV\",\"sw_version\":\"1.93.11\"},\"effect\":true,\"effect_list\":[\"blink\",\"breathe\",\"okay\",\"channel_change\",\"finish_effect\",\"stop_effect\"],\"json_attributes_topic\":\"zigbee2mqtt/Led Strip TV\",\"min_mireds\":150,\"name\":\"Led Strip TV\",\"schema\":\"json\",\"state_topic\":\"zigbee2mqtt/Led Strip TV\",\"supported_color_modes\":[\"xy\",\"color_temp\"],\"unique_id\":\"0x001788010bf4769e_light_zigbee2mqtt\"}"

-- >>> test1
-- Just (LightConfig {brightness = True, brightness_scale = Just 254, color_mode = True, command_topic = "zigbee2mqtt/Led Strip TV/set", device = Device {identifiers = ["zigbee2mqtt_0x001788010bf4769e"], manufacturer = "Philips", model = "Hue white and color ambiance LightStrip plus (8718699703424)", name = "Led Strip TV", sw_version = "1.93.11"}, effect = True, effect_list = ["blink","breathe","okay","channel_change","finish_effect","stop_effect"], json_attributes_topic = "zigbee2mqtt/Led Strip TV", max_mireds = Nothing, min_mireds = Just 150, name = "Led Strip TV", schema = "json", state_topic = "zigbee2mqtt/Led Strip TV", supported_color_modes = [XYMode,TemperatureMode], unique_id = "0x001788010bf4769e_light_zigbee2mqtt"})