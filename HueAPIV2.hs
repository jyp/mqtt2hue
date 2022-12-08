{-# LANGUAGE ViewPatterns #-}
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

module HueAPIV2 (module HueAPIV2, module ReExport) where


import MyAeson
import Data.Aeson
import GHC.Generics
import Servant
import Data.Time.Clock
import Data.Text (Text,pack,unpack,toUpper)
import Types
import HueAPI as ReExport (ColorGamutType(..)) 
import Data.List (intercalate)
import Servant.EventStream
import Data.Bits

type AppKey = Header "hue-application-key" Text 
type ClipV2 k = "clip" :> "v2" :> AppKey :> k
type HueApiV2
  =    "eventstream" :> "clip" :> "v2" :> AppKey :> StreamGet NewlineFraming EventStream (SourceIO SSE)
  :<|> ClipV2 ("resource" :>             Get '[JSON] (Response ResourceGet))
  :<|> ClipV2 ("resource" :> "bridge" :> Get '[JSON] (Response BridgeGet))
  :<|> ClipV2 ("resource" :> "bridge_home" :> Get '[JSON] (Response GroupGet))
  :<|> ClipV2 ("resource" :> "geolocation" :> Get '[JSON] (Response GeoLocationGet))
  :<|> ClipV2 ("resource" :> "geofence_client" :> Get '[JSON] (Response GeoFenceGet))
  :<|> ClipV2 ("resource" :> "behavior_instance" :> Get '[JSON] (Response Null))
  :<|> ClipV2 ("resource" :> "motion" :> Get '[JSON] (Response Null))
  :<|> ClipV2 ("resource" :> "zone" :> Get '[JSON] (Response GroupGet))
  :<|> ClipV2 ("resource" :> "room" :> Get '[JSON] (Response GroupGet))
  :<|> ClipV2 ("resource" :> "light" :> Get '[JSON] (Response LightGet))
  :<|> ClipV2 ("resource" :> "light" :> Capture "lightid" Identifier :> ReqBody '[JSON] LightPut :> Put '[JSON] Text)
  :<|> ClipV2 ("resource" :> "grouped_light" :> Capture "lightid" Identifier :> ReqBody '[JSON] LightPut :> Put '[JSON] Text)
 

newtype Identifier = Identifier Word128 deriving (Eq, Ord)

instance Bits Identifier where
  Identifier (Word128 a b) `xor` Identifier (Word128 a' b') =
    Identifier (Word128 (a `xor` a') (b `xor` b'))
  complement (Identifier (Word128 a b)) = Identifier (Word128 (complement a) (complement b))
  shift = error "Bits: Identifier: not implemented: shift"
  rotate = error "Bits: Identifier: not implemented: rotate"
  bitSize = error "Bits: Identifier: not implemented: bitSize"
  bitSizeMaybe = error "Bits: Identifier: not implemented: bitSizeMaybe"
  isSigned = error "Bits: Identifier: not implemented: isSigned"
  testBit = error "Bits: Identifier: not implemented: testBit"
  bit = error "Bits: Identifier: not implemented: bit"
  popCount = error "Bits: Identifier: not implemented: popCount"
  Identifier (Word128 a b) .&. Identifier (Word128 a' b') =
    Identifier (Word128 (a .&. a') (b .&. b'))

  Identifier (Word128 a b) .|. Identifier (Word128 a' b') =
    Identifier (Word128 (a .|. a') (b .|. b'))

fromInt :: Int -> Identifier
fromInt x = Identifier (Word128 0 (fromIntegral x))

identStore :: Identifier -> Int -> Identifier
identStore i x = (i .&. complement (fromInt 0xFFFF)) .|. fromInt x

identLoad :: Identifier -> Int
identLoad (Identifier (Word128 _ i)) = fromIntegral (i .&. 0xFFFF)

instance FromHttpApiData Identifier where
  parseUrlPiece x =
    if all (`elem` ['0'..'9'] ++ ['A'..'F']) y
    then Right (Identifier (Word128 (readHex h) (readHex l)))
    else Left "Invalid hue resource identifier"
    where (h,l) = splitAt 16 y
          y = filter (/= '-') (unpack (toUpper x))
          readHex z = read ("0x" ++ z)
  parseQueryParam _ = Left "Hue identifier cannot be passed as a query parameter"

instance Show Identifier where
  show (Identifier w) = intercalate "-" [a,b,c,d,e]
    where (splitAt 8 -> (a,
           splitAt 4 -> (b,
           splitAt 4 -> (c,
           splitAt 4 -> (d,
                         e))))) = show128 w

instance ToJSON Identifier where
  toJSON i = String (pack (show i))

data Brightness = Brightness { brightness :: Int } deriving (Eq,Show,Generic)
data ColorSet = ColorSet { xy :: XY } deriving (Show,Generic)
data Temperature = Temperature { mirek :: Int }deriving (Eq,Show,Generic)

type Path = Text

data EventData = DimmingEvent
   -- {"dimming":{"brightness":50.0},"id":"bca61a7f-0471-4ac0-9500-0a43d291179a","id_v1":"/lights/7","owner":{"rid":"6cc8c1fc-13d0-4f26-aafb-e2f8939fcd6a","rtype":"device"},"type":"light"},{"dimming":{"brightness":56.69},"id":"656089a4-df66-4415-9e73-ad0c4d4f82a5","id_v1":"/groups/0","owner":{"rid":"07bfb801-8250-4a40-8878-9d7d09ad0eed","rtype":"bridge_home"},"type":"grouped_light"},{"dimming":{"brightness":50.0},"id":"ac93e5d9-930a-43d9-97ab-fdfdc692ba99","id_v1":"/groups/7","owner":{"rid":"5275317f-dc25-4a07-a35e-3ff349de557d","rtype":"room"},"type":"grouped_light"}
                deriving (Generic,Show
                         )
data Event = Event
  {creationTime :: UTCTime
  ,_data :: [EventData]
  ,_type :: EventType
  ,_id :: Identifier
  } deriving (Show,Generic)

data EventType = UpdateEvent deriving Show
instance ToJSON EventType where
  toJSON = \case
    UpdateEvent -> "update"


data ResourceType
   = DeviceResource
   | BridgeHome
   | Room
   | Zone
   | LightResource
   | ButtonResource
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
  DeviceResource -> "device"
  BridgeHome -> "bridge_home"
  Room -> "room"
  Zone -> "zone"
  LightResource -> "light"
  ButtonResource -> "button"
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

data BridgeGet = BridgeGet
  { _id :: Identifier
  , id_v1 :: Path
  , owner :: Maybe ResourceRef
    -- ^ not listed in API v2 doc, but observed in data sent from Philips bridge
  , bridge_id :: Text
  , time_zone :: TimeZone
  , _type :: ResourceType
  }
data GroupGet = GroupGet {
      _id :: Identifier,
      id_v1 :: Path,
      children :: [ResourceRef],
      services :: [ResourceRef],
      metadata :: Maybe ArchetypeMeta,
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
   | Hallway
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
   Hallway -> "hallway"
   UnknownArchetype    -> "unknown_archetype"
   
data ProductData = ProductData {
        model_id :: Text,
        manufacturer_name :: Text,
        product_name :: Text,
        product_archetype :: Archetype,
        certified :: Bool,
        software_version :: Text,
        hardware_platform_type :: Maybe Text
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
instance FromJSON Dimming where parseJSON = myGenericParseJSON
data MirekSchema = MirekSchema {
          mirek_minimum :: Int,
          mirek_maximum :: Int
        }   deriving (Generic)
instance FromJSON MirekSchema where parseJSON = myGenericParseJSON
data ColorTempGet = ColorTempGet {
        mirek :: Int,
        mirek_valid :: Bool, -- true if light is in full color mode
        mirek_schema :: MirekSchema
      } deriving (Generic)
instance FromJSON ColorTempGet where parseJSON = myGenericParseJSON
data XY = XY {x, y :: Float} deriving (Generic, Show)
instance FromJSON XY where parseJSON = myGenericParseJSON
data Gamut = Gamut {red , green , blue :: XY} deriving (Generic,Show)
instance FromJSON Gamut where parseJSON = myGenericParseJSON
data ColorGet = ColorGet {
        xy :: XY,
        gamut :: Maybe Gamut,
        gamut_type:: Maybe ColorGamutType
      } deriving (Generic)
instance FromJSON ColorGet where parseJSON = myGenericParseJSON

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
      dimming :: Maybe Dimming,
      -- "dimming_delta": {},
      color_temperature :: Maybe ColorTempGet,
      -- "color_temperature_delta": {},
      color :: Maybe ColorGet,
      dynamics :: Dynamics,
      alert:: Alert,
      -- "signaling": {},
      mode:: Text, -- "normal"
      effects :: Maybe Effects,
      _type :: ResourceType
    } deriving Generic
data LightPut = LightPut
  {
      on :: Maybe IsOn,
      dimming :: Maybe Dimming,
      color_temperature :: Maybe ColorTempSet,
      color :: Maybe ColorGet
  } deriving Generic
instance FromJSON LightPut where parseJSON = myGenericParseJSON
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


data UpdatePut = UpdatePut
  {install::Bool
  ,autoinstall::IsOn
  } deriving Generic
instance FromJSON UpdatePut
data ConfigPut = ConfigPut
  {
    swupdate2 :: UpdatePut
  } deriving Generic
instance FromJSON ConfigPut

data GeoLocationGet = GeoLocationGet {
  _id :: Identifier,
  is_configured :: Bool,
  _type :: ResourceType
  }

data GeoFenceGet = GeoFenceGet {
  _id :: Identifier,
  name :: Text,
  _type :: ResourceType
  }

data ResourceGet
  = RBridge BridgeGet
  | RLight LightGet
  | RGroup GroupGet
  | RDevice DeviceGet
  | RGeoLoc GeoLocationGet
  | RGeoFence GeoFenceGet
  deriving (Generic)
data Error = Error { description :: Text }  deriving (Generic)
data Response a = Response
  { _data :: [a]
  , errors :: [Error]
  } 
instance ToJSON Brightness
instance ToJSON ColorGet
instance ToJSON ColorSet
instance ToJSON Temperature
instance ToJSON EventData
instance ToJSON Event
instance FromJSON IsOn
instance ToJSON IsOn
instance ToJSON Dimming
instance ToJSON ColorTempSet
instance FromJSON ColorTempSet
instance ToJSON ColorTempGet
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
$(myDeriveToJSON ''GroupGet)
$(myDeriveToJSON ''GeoLocationGet)
$(myDeriveToJSON ''GeoFenceGet)
$(myDeriveToJSON ''Response)
instance ToJSON ResourceGet where toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue}



