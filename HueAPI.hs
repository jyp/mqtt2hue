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

module HueAPI where

import Prelude ()
import Prelude.Compat
import MyAeson
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Servant
import Data.Map
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text

type HueApi =    "api" :> ReqBody '[JSON] CreateUser :> Post '[JSON] [CreatedUser]
           :<|>  "api" :>                            "config" :> Get '[JSON] Config
           :<|>  "api" :> Capture "userid" String :>             Get '[JSON] Everything
           :<|>  "api" :> Capture "userid" String :> "config" :> Get '[JSON] Config
           :<|>  "api" :> Capture "userid" String :> "lights" :> Get '[JSON] (Map Int Light)
           :<|>  "api" :> Capture "userid" String :> "groups" :> Get '[JSON] (Map Int Group)
           :<|>  "api" :> Capture "userid" String :> "groups" :> Capture "groupid" Int :> Get '[JSON] Group
           :<|>  "api" :> Capture "userid" String :> "groups" :> Capture "groupid" Int :> "action" :> ReqBody '[JSON] Action :> Put '[JSON] Text

data CreatedUser = CreatedUser {success :: UserName}
  deriving (Eq, Show, Generic)
data UserName = UserName {username :: Text}
  deriving (Eq, Show, Generic)
data CreateUser = CreateUser {devicetype :: Text }  -- , generate_clientkey :: Bool
  deriving (Eq, Show, Generic)


instance FromJSON CreateUser
instance ToJSON CreatedUser
instance ToJSON UserName



data SwUpdate = SwUpdate {state :: Updates, lastinstall :: TimeStamp}
  deriving (Eq, Show, Generic)
instance ToJSON SwUpdate
data CfgUpdate1 = CfgUpdate1 {
      updatestate:: Int,
      checkforupdate:: Bool,
      devicetypes:: DeviceTypes ,
      url:: String,
      text:: String,
      notify:: Bool
      }
  deriving (Eq, Show, Generic)
instance ToJSON CfgUpdate1
data DeviceTypes =  DeviceTypes {
          bridge:: Bool,
          lights:: [Int],
          sensors:: [Int]
          }
  deriving (Eq, Show, Generic)
instance ToJSON DeviceTypes
data Updates = NoUpdates
  deriving (Eq, Show, Generic)
instance ToJSON Updates where
  toJSON = \case
    NoUpdates -> "noupdates"

data Light = Light { state :: LightState
                   , swupdate :: SwUpdate
                   , _type :: LightType
                   , name :: Text
                   , modelid :: Text
                   , manufacturername :: Text
                   , productname :: Text
                   , capabilities :: Capabilities
                   , config :: LightConfig
                   , uniqueid :: Text
                   , swversion :: Text
                   -- "swconfigid": "2435DF32",
                   -- "productid": "Philips-LCL001-1-LedStripsv4"
                   }
  deriving (Eq, Show)

instance ToJSON Effect
instance ToJSON Alert
instance ToJSON ColorMode where
  toJSON = \case
     CT -> "ct"
     XY -> "xy"
instance ToJSON LightMode where
  toJSON = \case
    HomeAutomation -> "homeautomation"
instance ToJSON Streaming
instance ToJSON Ct
instance ToJSON ColorGamutType where
  toJSON = \case
    C -> "C"
    Other -> "other"
instance ToJSON LightConfig
instance ToJSON Startup
  
data LightType = DimmableLight | TemperatureLight | ExtendedColorLight
  deriving (Eq, Show, Generic)
instance ToJSON LightType where
  toJSON = \case
    DimmableLight -> "Dimmable light"
    TemperatureLight -> "Color temperature light"
    ExtendedColorLight -> "Extended color light"

data LightState = LightState
  {on :: Bool
  ,bri :: Int
  -- ,hue :: Maybe Int -- FIXME
  -- ,sat :: Maybe Int
  ,ct :: Maybe Int
  ,effect :: Effect
  ,xy :: Maybe [Float]
  ,alert :: Alert
  ,colorMode :: Maybe ColorMode
  ,mode :: LightMode
  ,reachable :: Bool}
  deriving (Eq, Show, Generic)
data Action = Action {
  on :: Maybe Bool
  ,bri :: Maybe Int
  ,xy :: Maybe [Float]
  ,ct :: Maybe Int
  } deriving (Eq, Show, Generic)
instance FromJSON Action

data Alert = Select
  deriving (Eq, Show, Generic)
data Effect = None
  deriving (Eq, Show, Generic)
data ColorMode = CT | XY
  deriving (Eq, Show, Generic)

data LightMode = HomeAutomation
  deriving (Eq, Show, Generic)
data LightConfig = LightConfig { archetype :: String
                               , function :: String
                               , direction :: String
                               , startup :: Startup}
  deriving (Eq, Show, Generic)
data Startup = Startup { mode :: StartupMode, configured :: Bool}
  deriving (Eq, Show, Generic)
data StartupMode = Safety | Powerfail
  deriving (Eq, Show, Generic)
instance ToJSON StartupMode where
  toJSON = \case
     Safety -> "safety"
     Powerfail -> "powerfail"
data Capabilities = Capabilities
  { certified :: Bool,
    control :: Control
  }
  deriving (Eq, Show, Generic)
data Streaming = Streaming { renderer :: Bool, proxy :: Bool}
  deriving (Eq, Show, Generic)

data Control = NoControl | Ct {ct :: Ct} | FullColor {colorgamuttype :: ColorGamutType, colorgamut :: Maybe [[Float]]} 
  deriving (Eq, Show, Generic)
data ColorGamutType = C | Other
  deriving (Eq, Show, Generic)
data Ct = CtValues {min :: Int,  max :: Int}
  deriving (Eq, Show, Generic)
data Group = Group
  { name :: String
  , lights :: [String]
  , sensors :: [String]
  , _type :: GroupType
  , state :: GroupState
  , recycle ::  Bool
  , _class :: Maybe String
  , action :: LightState
  -- , precence :: Presence
  -- , lightlevel :: LightLevel
  }
  deriving (Eq, Show, Generic)


data Class = Office | Bedroom | Garage | LivingRoom | Hallway | Kitchen | Attic
  deriving (Eq, Show, Generic)
data GroupType = Room | LightGroup
  deriving (Eq, Show, Generic)
data GroupState = GroupState { all_on, any_on :: Bool}
  deriving (Eq, Show, Generic)

data Config = Config
  { name :: String,
    datastoreversion :: String,
    swversion :: String,
    bridgeid :: String,
    mac :: String,
    factorynew :: Bool,

    zigbeechannel :: Int,
    dhcp :: Bool,
    ipaddress :: String,
    netmask :: String,
    gateway :: String,
    proxyaddress :: String,
    proxyport :: Int,
    _UTC :: TimeStamp,
    localtime :: TimeStamp,
    timezone :: String,
    modelid :: String,
    apiversion :: String,
    swupdate :: CfgUpdate1,
    swupdate2 :: CfgUpdate2 ,
    linkbutton :: Bool,
    portalservices :: Bool,
    portalconnection :: Connection,
    portalstate :: PortalState ,
    internetservices :: InternetServices,
    replacesbridgeid :: Null,
    backup :: Backup,
    starterkitid :: String,
    whitelist :: [WhiteListEntry]
  } deriving (Eq, Show, Generic)

data Null = Null
  deriving (Eq, Show, Generic)
instance ToJSON Null where
  toJSON HueAPI.Null = Data.Aeson.Types.Null

data CfgUpdate2 = CfgUpdate2 {
    checkforupdate :: Bool,
    lastchange :: TimeStamp,
    bridge :: BridgeUpdate,
    state :: Updates,
    autoinstall :: AutoInstall
  }
  deriving (Eq, Show, Generic)
instance ToJSON CfgUpdate2

data BridgeUpdate = BridgeUpdate {
      state:: Updates,
      lastinstall:: TimeStamp
    }
  deriving (Eq, Show, Generic)
instance ToJSON BridgeUpdate
data AutoInstall = AutoInstall {
      updatetime:: TimeOfDay,  
      on:: Bool
    }
  deriving (Eq, Show, Generic)
instance ToJSON AutoInstall

data PortalState = PortalState {
    signedon :: Bool,
    incoming :: Bool,
    outgoing :: Bool,
    communication :: Connection
  }
  deriving (Eq, Show, Generic)
instance ToJSON PortalState

data InternetServices = InternetServices {
    internet :: Connection,
    remoteaccess :: Connection,
    time :: Connection,
    swupdate :: Connection
  }
  deriving (Eq, Show, Generic)
instance ToJSON InternetServices
data Connection = Connected | Disconnected
  deriving (Eq, Show, Generic)
instance ToJSON Connection where
  toJSON = \case
    Connected -> "connected"
    Disconnected -> "disconnected"
data Backup = Backup { status :: Status, errorcode :: Int}
  deriving (Eq, Show, Generic)
instance ToJSON Backup
data Status = Idle
  deriving (Eq, Show, Generic)
instance ToJSON Status where
  toJSON = \case
    Idle -> "idle"
data WhiteListEntry = WhiteListEntry
  deriving (Eq, Show, Generic)
instance ToJSON WhiteListEntry
type TimeStamp = UTCTime

data Dummy = Dummy deriving (Eq, Show, Generic)
instance ToJSON Dummy
data Everything = Everything
  {lights :: Map Int Light
  ,groups :: Map Int Group
  ,config :: Config
  ,schedules :: Map Int Dummy
  ,scenes :: Map Int Dummy
  ,rules :: Map Int Dummy
  ,sensors :: Map Int Dummy
  ,resoucelinks :: Map Int Dummy
  }
  deriving (Eq, Show, Generic)


instance ToJSON GroupType
instance ToJSON GroupState
$(myDeriveToJSON ''Control)
instance ToJSON Capabilities
$(myDeriveToJSON ''LightState)
$(myDeriveToJSON ''Light)
$(myDeriveToJSON ''Group)
$(myDeriveToJSON ''Config)
instance ToJSON Everything
