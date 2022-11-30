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

import HueAPI (ColorGamut)

type HueApiV2
  =  "eventstream" :> "clip" :> "v2" :> StreamGet NewlineFraming JSON (SourceIO Event)
  <|> "clip" :> "v2" :> "resource" :> "bridge" :> Get '[JSON] BridgeGet
 
-- TODO /clip/v2/resource

instance ToJSON ResourceType where
  toJSON = \case
    LightRes -> "light"
    SensorRes -> "sensor"

data Brightness = Brightness { brightness :: Int } deriving (Eq,Show,Generic)
data ColorSet = ColorSet { xy :: XY }deriving (Eq,Show,Generic)
data Temperature = Temperature { mirek :: Int }deriving (Eq,Show,Generic)

data Event = Event {
  resource :: ResourceType,
  idv1 :: Path,
  _id :: Text,
  creationTime :: UTCTime,
  dimming :: Brightness,
  color :: Maybe ColorSet,
  color_temperature :: Maybe Temperature
} deriving (Eq,Show,Generic)

data ResourceType
   = Device
   | BridgeHome
   | Room
   | Zone
   | Light
   | Button
   | Temperature
   | LightLevel
   | Motion
   | Entertainment
   | GroupedLight
   | DevicePower
   | ZigbeeBridgeConnectivity
   | ZigbeeConnectivity
   | ZgpConnectivity
   | Bridge
   | Homekit
   | Scene
   | EntertainmentConfiguration
   | PublicImage
   | AuthV1
   | BehaviorScript
   | BehaviorInstance
   | Geofence
   | GeofenceClient
   | Geolocation



instance ToJSON ResourceType where
 toJSON x = String $ case x of
  Device -> "device"
  BridgeHome -> "bridge_home"
  Room -> "room"
  Zone -> "zone"
  Light -> "light"
  Button -> "button"
  Temperature -> "temperature"
  LightLevel -> "light_level"
  Motion -> "motion"
  Entertainment -> "entertainment"
  GroupedLight -> "grouped_light"
  DevicePower -> "device_power"
  ZigbeeBridgeConnectivity -> "zigbee_bridge_connectivity"
  ZigbeeConnectivity -> "zigbee_connectivity"
  ZgpConnectivity -> "zgp_connectivity"
  Bridge -> "bridge"
  Homekit -> "homekit"
  Scene -> "scene"
  EntertainmentConfiguration -> "entertainment_configuration"
  PublicImage -> "public_image"
  AuthV1 -> "auth_v1"
  BehaviorScript -> "behavior_script"
  BehaviorInstance -> "behavior_instance"
  Geofence -> "geofence"
  GeofenceClient -> "geofence_client"
  Geolocation -> "geolocation"

data TimeZone = TimeZone
  { time_zone :: Text
  }
data ResourceRef = ResourceRef
  {rid :: Identifier
  ,rtype :: ResourceType
  }
type Identifier = Text
data BridgeGet = BridgeGet
  { _id :: Identifier
  , id_v1 :: Path
  , owner :: ResourceRef
  , bridge_id :: Text
  , time_zone :: TimeZone
  , _type :: ResourceType
  }
data GroupGet = GroupGet {
      _id :: Identifier,
      id_v1 :: Path,
      children :: [ResourceRef],
      services :: [ResourceRef],
      metadata :: ArchetypeMeta,
      _type :: ResourceType
      }
data ArchetypeMeta = ArchetypeMeta {
        name :: Text,
        archetype :: Archetype
      }

data Archetype
   = Attic
   | Bedroom
   | BridgeV2
   | CeilingRound
   | CeilingSquare
   | ClassicBulb
   | DoubleSpot
   | Garage
   | HuePlay
   | Kitchen
   | LivingRoom
   | Office
   | SingleSpot
   | SultanBulb
   | VintageBulb
   | UnknownArchetype

instance ToJSON Archetype where
  toJSON x = case x of
   Attic               -> "attic" 
   Bedroom             -> "bedroom" 
   BridgeV2            -> "bridge_v2" 
   CeilingRound        -> "ceiling_round" 
   CeilingSquare       -> "ceiling_square" 
   ClassicBulb         -> "classic_bulb" 
   DoubleSpot          -> "double_spot" 
   Garage              -> "garage" 
   HuePlay             -> "hue_play" 
   Kitchen             -> "kitchen" 
   LivingRoom          -> "living_room" 
   Office              -> "office" 
   SingleSpot          -> "single_spot" 
   SultanBulb          -> "sultan_bulb" 
   VintageBulb         -> "vintage_bulb" 
   UnknownArchetype    -> "unknown_archetype"
   
data ProductData = ProductData {
        model_id :: Text,
        manufacturer_name :: Text,
        product_name :: Text,
        product_archetype :: Archetype,
        certified :: Bool,
        software_version :: Text,
        hardware_platform_type :: Text
      }

data DeviceGet = DeviceGet {
      _id :: Identifier,
      id_v1 :: Path,
      product_data :: ProductData,
      metadata :: ArchetypeMeta,
      services :: [ResourceRef],
      _type :: ResourceType
    }
data IsOn = IsOn {on :: Bool}
data Dimming = Dimming {
        brightness :: Float,
        min_dim_level :: Maybe Float
      }
data MirekSchema = MirekSchema {
          mirek_minimum :: Int,
          mirek_maximum :: Int
        }  
data ColorTemp = ColorTemp {
        -- "mirek": null,
        mirek_valid :: Bool,
        mirek_schema :: MirekSchema
      }
data XY = XY {x, y :: Float}
data Gamut = Gamut {red , green , blue :: XY}  
data ColorGet = ColorGet {
        xy :: XY,
        gamut :: Gamut,
        gamut_type:: ColorGamutType
      }
              
data Dynamics = Dynamics {
        status :: Text, -- "none",
        status_values :: [Text],
        speed :: Float,
        speed_valid :: Bool
      }
data Alert = Alert {action_values :: [Text]}
data Effects = Effects {
        status_values :: [String],
        status :: Text,
        effect_values :: [Text]
      }          
data Light = Light {
      _id :: Identifier,
      id_v1 :: Path,
      -- owner is light device
      owner :: ResourceRef,
      metadata :: ArchetypeMeta,
      on :: IsOn,
      dimming :: Dimming,
      -- "dimming_delta": {},
      color_temperature :: ColorTemp,
      -- "color_temperature_delta": {},
      color :: Color,
      dynamics :: Dynamics,
      alert:: Alert,
      -- "signaling": {},
      mode:: Text, -- "normal"
      effects :: Effects,
      _type :: ResourceType
    }
data SceneMeta = SceneMeta {
  name :: Text
  -- image: ResourceRef
  }

data ColorTempSet = ColorTempSet {
        mirek :: Int
        }
  
data Act =  Act {
  on:: IsOn,
  dimming :: Dimming,
  color_temperature :: Maybe ColorTempSet,
  color_temperature :: Maybe ColorSet
  }
data Action = Action {
          target :: ResourceRef,
          action:: Act
        }
data Scene = Scene {
      _id :: Identifier,
      id_v1 :: Path,
      metadata :: SceneMeta,
      group :: ResourceRef,
      actions :: [Action],
      -- "palette": ,
      speed :: Float, -- in 0,1
      auto_dynamic :: Bool,
      _type :: ResourceType
    }


data Error = Error { description :: Text }
data Hue2Reply a = Hue2Reply
  { _data :: [a]
  , errors :: [Error]
  }


instance ToJSON Error
instance ToJSON ResourceRef
instance ToJSON TimeZone
$(myDeriveJSON ''BridgeGet)
$(myDeriveJSON ''Hue2Reply)
instance ToJSON Brightness
instance ToJSON Color
instance ToJSON Temperature
instance ToJSON Event


