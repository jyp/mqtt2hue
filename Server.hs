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
module Server where

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
import Types


server1 :: Server HueApi
server1 =    bridgeConfig
        :<|> configuredLights

serverConfig = ServerConfig { mac = "90:61:ae:21:8f:6d"
                            ,ipaddress = "192.168.1.50"
                            ,netmask = "255.255.255.0"
                            ,gateway = "192.168.1.1"
                            }

bridgeConfig :: String -> Handler Config
bridgeConfig userId = do
 now <- liftIO getCurrentTime
 return $ Config
  {name = "Philips hue"
  ,zigbeechannel = 15
  ,bridgeid = "ECB5FAFFFE259802"
  ,dhcp = True
  ,proxyaddress = "none"
  ,proxyport = 0
  ,utc = now
  ,localtime = now
  ,timezone = "Europe/Stockholm"
  ,modelid = "BSB002"
  ,datastoreversion = "131"
  ,swversion = "1953188020"
  ,apiversion = "1.53.0"
  ,swupdate = CfgUpdate1 {updatestate = 0
                         ,checkforupdate = False
                         ,devicetypes = DeviceTypes {bridge = False
                                                    ,lights = []
                                                    ,sensors = []
                                                    }
                         ,url = ""
                         ,text = ""
                         ,notify = True
                         }
  ,swupdate2 = CfgUpdate2 {checkforupdate = False
                           ,lastchange = now
                           ,bridge = BridgeUpdate {state = NoUpdates
                                                  ,lastinstall = now
                                                  }
                           ,state = NoUpdates
                           ,autoinstall = AutoInstall {updatetime = TimeOfDay 3 0 0
                                                      ,on = True }
                           }
  ,linkbutton = False
  ,portalservices = True
  ,portalconnection = Disconnected
  ,portalstate = PortalState {signedon = False
                             ,incoming = False
                             ,outgoing = False
                             ,communication = Disconnected
                             }
  ,internetservices = InternetServices {internet = Connected
                                       ,remoteaccess = Connected
                                       ,time = Connected
                                       ,swupdate = Disconnected
                                       }
  ,factorynew = False
  ,replacesbridgeid = Types.Null
  ,backup = Backup {status = Idle
                   ,errorcode = 0
                   }
  ,starterkitid = ""
  ,whitelist = []
  , ..
  } where ServerConfig {..} = serverConfig

configuredLights :: String -> Handler (IntMap Light)
configuredLights _ = return $
  fromList [(1,Light {state = LightState {}
                     ,swUpdate = SwUpdate {}
                     ,_type = "Color temperature light"
                     ,name = "Hue ambiance lamp in my office"
                     ,modeLid = "LTW010" -- ?
                     ,manufacturerName = "Signify"
                     ,productName = "Hue ambiance lamp"
                     ,capabilities = Capabilities
                       { certified = False,
                         control = Ct {}
                       }
                     ,config = LightConfig {
                         archetype = "sultanbulb",
                         function = "functional",
                         direction = "omnidirectional",
                         startup = Startup {}}
                     ,uniqueid = _
                     ,swversion = _})]

app1 :: Application
app1 = serve (Proxy @HueApi) server1

main :: IO ()
main = run 8081 app1

