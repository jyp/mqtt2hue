{-# LANGUAGE EmptyCase #-}
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
{-# LANGUAGE TemplateHaskell #-}

module HueAPIV2 where


import Prelude ()
import Prelude.Compat
import MyAeson
import Data.Aeson
import GHC.Generics
import Servant
import Data.Time.Clock
import Data.Text

import MQTTAPI (ColorXY(..))

type HueApiV2 = "eventstream" :> "clip" :> "v2" :> StreamGet NewlineFraming JSON (SourceIO Event)
 
  -- TODO /clip/v2/resource

data ResourceType = LightRes | SensorRes deriving (Eq,Show,Generic)
instance ToJSON ResourceType where
  toJSON = \case
    LightRes -> "light"
    SensorRes -> "sensor"

data Brightness = Brightness { brightness :: Int } deriving (Eq,Show,Generic)
data Color = Color { xy :: ColorXY }deriving (Eq,Show,Generic)
data Temperature = Temperature { mirek :: Int }deriving (Eq,Show,Generic)

data Event = Event {
  resource :: ResourceType,
  idv1 :: String,
  idv2 :: String,
  creationTime :: UTCTime,
  dimming :: Brightness,
  color :: Maybe Color,
  color_temperature :: Maybe Temperature
} deriving (Eq,Show,Generic)

data BridgeGet = BridgeGet
  { _id :: Text
  }

instance ToJSON Brightness
instance ToJSON Color
instance ToJSON Temperature
instance ToJSON Event

