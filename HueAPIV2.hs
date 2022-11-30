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

import HueAPI (ColorGamutType(..))

type HueApiV2
  =    "eventstream" :> "clip" :> "v2" :> StreamGet NewlineFraming JSON (SourceIO Event)
  :<|> "clip" :> "v2" :> "resource" :> "bridge" :> Get '[JSON] BridgeGet
 
-- TODO /clip/v2/resource

data Brightness = Brightness { brightness :: Int } deriving (Eq,Show,Generic)
data ColorSet = ColorSet { xy :: XY }deriving (Show,Generic)
data Temperature = Temperature { mirek :: Int }deriving (Eq,Show,Generic)

type Path = Text

data Event = Event {
  resource :: ResourceType,
  idv1 :: Path,
  _id :: Text,
  creationTime :: UTCTime,
  dimming :: Brightness,
  color :: Maybe ColorSet,
  color_temperature :: Maybe Temperature
} deriving (Show,Generic)

data ResourceType
   = Device
   | BridgeHome
   | Room
   | Zone
   | LightResource
   | Button
   | TemperatureResource
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
  deriving Show


instance ToJSON ResourceType where
 toJSON x = String $ case x of
  Device -> "device"
  BridgeHome -> "bridge_home"
  Room -> "room"
  Zone -> "zone"
  LightResource -> "light"
  Button -> "button"
  TemperatureResource -> "temperature"
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
  } deriving (Show, Generic)
data ResourceRef = ResourceRef
  {rid :: Identifier
  ,rtype :: ResourceType
  } deriving Generic
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
      } deriving (Show, Generic)
instance ToJSON ArchetypeMeta

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
  deriving (Show, Eq)
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
      } deriving (Generic)

data DeviceGet = DeviceGet {
      _id :: Identifier,
      id_v1 :: Path,
      product_data :: ProductData,
      metadata :: ArchetypeMeta,
      services :: [ResourceRef],
      _type :: ResourceType
    }
data IsOn = IsOn {on :: Bool}
 deriving (Generic)
data Dimming = Dimming {
        brightness :: Float,
        min_dim_level :: Maybe Float
      }
  deriving (Generic)            
data MirekSchema = MirekSchema {
          mirek_minimum :: Int,
          mirek_maximum :: Int
        }   deriving (Generic)
data ColorTemp = ColorTemp {
        -- "mirek": null,
        mirek_valid :: Bool,
        mirek_schema :: MirekSchema
      } deriving (Generic)
data XY = XY {x, y :: Float} deriving (Generic, Show)
data Gamut = Gamut {red , green , blue :: XY} deriving (Generic,Show)
data ColorGet = ColorGet {
        xy :: XY,
        gamut :: Gamut,
        gamut_type:: ColorGamutType
      } deriving (Generic)
     
data Dynamics = Dynamics {
        status :: Text, -- "none",
        status_values :: [Text],
        speed :: Float,
        speed_valid :: Bool
      } deriving (Generic)
data Alert = Alert {action_values :: [Text]} deriving (Generic)
data Effects = Effects {
        status_values :: [String],
        status :: Text,
        effect_values :: [Text]
      } deriving (Generic)
data LightGet = LightGet {
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
      color :: ColorGet,
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
  } deriving (Generic)

data ColorTempSet = ColorTempSet {
        mirek :: Int
        } deriving (Generic)
  
data Act =  Act {
  on:: IsOn,
  dimming :: Dimming,
  color_temperature :: Maybe ColorTempSet,
  color_temperature :: Maybe ColorSet
  } deriving (Generic)
data Action = Action {
          target :: ResourceRef,
          action:: Act
        } deriving (Generic)
data SceneGet = SceneGet {
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


data Error = Error { description :: Text }  deriving (Generic)
data Hue2Reply a = Hue2Reply
  { _data :: [a]
  , errors :: [Error]
  } 

instance ToJSON Brightness
instance ToJSON ColorGet
instance ToJSON ColorSet
instance ToJSON Temperature
instance ToJSON Event
instance ToJSON IsOn
instance ToJSON Dimming
instance ToJSON ColorTempSet
instance ToJSON ColorTemp
instance ToJSON Action
instance ToJSON Dynamics
instance ToJSON MirekSchema
instance ToJSON Alert
instance ToJSON Act
instance ToJSON Effects
instance ToJSON ProductData
instance ToJSON SceneMeta
instance ToJSON XY
instance ToJSON Gamut
instance ToJSON ResourceRef
instance ToJSON TimeZone
instance ToJSON Error
$(myDeriveToJSON ''BridgeGet)
$(myDeriveToJSON ''SceneGet)
$(myDeriveToJSON ''LightGet)
$(myDeriveToJSON ''DeviceGet)
$(myDeriveToJSON ''Hue2Reply)


