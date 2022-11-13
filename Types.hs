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

module Types where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.IntMap
import Data.Time.Clock
import Data.Time.LocalTime

type HueApi =    "api" :> Capture "userid" String :> "config" :> Get '[JSON] Config
           :<|>  "api" :> Capture "userid" String :> "lights" :> Get '[JSON] (IntMap Light)


data SwUpdate = SwUpdate {state :: Updates, lastinstall :: String}
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
instance ToJSON Updates

data Light = Light { state :: LightState
                   , swUpdate :: SwUpdate
                   , _type :: String
                   , name :: String
                   , modeLid :: String
                   , manufacturerName :: String
                   , productName :: String
                   , capabilities :: Capabilities
                   , config :: LightConfig
                   , uniqueid :: String
                   , swversion :: String}
  deriving (Eq, Show)
instance ToJSON Light where
  toJSON (Light {..}) =
        object ["state" .= state
               ,"swUpdate" .= swUpdate 
               ,"type" .= _type 
               ,"name" .= name 
               ,"modeLid" .= modeLid 
               ,"manufacturerName" .= manufacturerName 
               ,"productName" .= productName 
               ,"capabilities" .= capabilities 
               ,"config" .= config 
               ,"uniqueid" .= uniqueid 
               ,"swversion" .= swversion]

instance ToJSON LightState
instance ToJSON Effect
instance ToJSON Alert
instance ToJSON ColorMode
instance ToJSON LightMode
instance ToJSON Capabilities
instance ToJSON Streaming
instance ToJSON Control
instance ToJSON Ct
instance ToJSON ColorGamutType
instance ToJSON LightConfig
instance ToJSON Startup
instance ToJSON StartupMode
data LightType = Dimmable | ColorTemperature | ExtendedColorLight
  deriving (Eq, Show, Generic)
data LightState = LightState
  {on :: Bool
  ,bri :: Int
  ,hue :: Int
  ,sat :: Int
  ,ct :: Int
  ,effect :: Effect
  ,xy :: [Float]
  ,alert :: Alert
  ,colorMode :: ColorMode
  ,mode :: LightMode
  ,reachable :: Bool}
  deriving (Eq, Show, Generic)
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
data StartupMode = Safety
  deriving (Eq, Show, Generic)
data Capabilities = Capabilities
  { certified :: Bool,
    control :: Control
  }
  deriving (Eq, Show, Generic)
data Streaming = Streaming { renderer :: Bool, proxy :: Bool}
  deriving (Eq, Show, Generic)

data Control = NoControl | Ct {ct :: Ct} | FullColor {colorgamuttype :: ColorGamutType, colorgamut :: [[Float]]} 
  deriving (Eq, Show, Generic)
data ColorGamutType = C
  deriving (Eq, Show, Generic)
data Ct = CtValues {min :: Int,  max :: Int}
  deriving (Eq, Show, Generic)
data Group = Group
  { name :: String
  , lights :: [Int]
  , sensors :: [Int]
  , _type :: GroupType
  , state :: GroupState
  , recycle ::  Bool
  , _class :: String
  , action :: LightState
  }
  deriving (Eq, Show, Generic)
data Class = Office | Bedroom | Garage | LivingRoom | Hallway | Kitchen | Attic
  deriving (Eq, Show, Generic)
data GroupType = Room
  deriving (Eq, Show, Generic)
data GroupState = GroupState { all_on, any_on :: Bool}
  deriving (Eq, Show, Generic)

data Config = Config
  { name :: String,
    zigbeechannel :: Int,
    bridgeid :: String,
    mac :: String,
    dhcp :: Bool,
    ipaddress :: String,
    netmask :: String,
    gateway :: String,
    proxyaddress :: String,
    proxyport :: Int,
    utc :: TimeStamp,
    localtime :: TimeStamp,
    timezone :: String,
    modelid :: String,
    datastoreversion :: String,
    swversion :: String,
    apiversion :: String,
    swupdate :: CfgUpdate1,
    swupdate2 :: CfgUpdate2 ,
    linkbutton :: Bool,
    portalservices :: Bool,
    portalconnection :: Connection,
    portalstate :: PortalState ,
    internetservices :: InternetServices,
    factorynew :: Bool,
    replacesbridgeid :: Null,
    backup :: Backup,
    starterkitid :: String,
    whitelist :: [WhiteListEntry]
  } deriving (Eq, Show, Generic)
instance ToJSON Config
data Null = Null
  deriving (Eq, Show, Generic)
instance ToJSON Null

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
data Backup = Backup { status :: BackupStatus, errorcode :: Int}
  deriving (Eq, Show, Generic)
instance ToJSON Backup
data BackupStatus = Idle
  deriving (Eq, Show, Generic)
instance ToJSON BackupStatus
data WhiteListEntry = WhiteListEntry
  deriving (Eq, Show, Generic)
instance ToJSON WhiteListEntry
type TimeStamp = UTCTime



data ServerConfig = ServerConfig { mac :: String,
                                   ipaddress, netmask, gateway :: String }

