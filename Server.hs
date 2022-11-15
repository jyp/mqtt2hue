{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
import Data.ByteString.Lazy.Char8
import Data.Aeson.Text
-- import Data.List
import Data.Map
import Data.Maybe
import Data.String.Conversions
-- import Data.Time.Calendar
-- import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
-- import GHC.Generics
-- import Lucid
-- import Network.HTTP.Media ((//), (/:))
import Network.MQTT.Client
import Network.MQTT.Topic
-- import Network.MQTT.Types
import Network.URI
import Network.Wai
-- import Servant.Types.SourceT (source)
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html
-- import Network.Wai.Handler.Warp
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO
import qualified Data.Text.IO as Text
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Servant.Types.SourceT as S

import Prelude ()
import Prelude.Compat
import Servant
import Logic
import Types
import HueAPI
import qualified HueAPIV2
import MQTTAPI


type HueHandler = ReaderT (MVar ServerState) Handler

hueServerV1 :: ServerT HueApi HueHandler
hueServerV1 =  createUser
        :<|> bridgePublicConfig
        :<|> allConfig
        :<|> bridgeConfig
        :<|> configuredLights
        :<|> configuredGroups
        :<|> configuredGroup

hueServerV2 :: ServerT HueAPIV2.HueApiV2 HueHandler
hueServerV2 = return (S.fromStepT s)
  where s = S.Effect (threadDelay 1000000 >> return s)

hueServer :: ServerT (HueApi :<|> HueAPIV2.HueApiV2) HueHandler
hueServer = hueServerV1 :<|> hueServerV2

serverConfig :: ServerConfig
serverConfig = ServerConfig { mac = "90:61:ae:21:8f:6d"
                            , ipaddress = "192.168.1.50"
                            , netmask = "255.255.255.0"
                            , gateway = "192.168.1.1"
                            }

bridgeConfig :: String -> HueHandler Config
bridgeConfig _userId = bridgePublicConfig

createUser :: CreateUser -> HueHandler [CreatedUser]
createUser CreateUser {..} = do
  liftIO (Text.putStrLn ("user-creation requested for " <> devicetype))
  return [CreatedUser $ UserName "83b7780291a6ceffbe0bd049104df"] -- FIXME

bridgePublicConfig :: HueHandler Config
bridgePublicConfig = do
 now <- liftIO getCurrentTime
 return $ Config
  {name = "MQTT2hue" -- "Philips Hue"
  ,zigbeechannel = 15
  ,bridgeid = "ECB5FAFFFE259802" -- ??
  ,dhcp = True
  ,proxyaddress = "none"
  ,proxyport = 0
  ,_UTC = now
  ,localtime = now
  ,timezone = "Europe/Stockholm"
  ,modelid = "BSB001"
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
  ,replacesbridgeid = HueAPI.Null
  ,backup = Backup {status = HueAPI.Idle
                   ,errorcode = 0
                   }
  ,starterkitid = ""
  ,whitelist = []
  , ..
  } where ServerConfig {..} = serverConfig

askingState :: ToJSON a => (ServerState -> a) -> HueHandler a
askingState f = do
  st <- liftIO . readMVar =<< ask
  let x = f st
  liftIO (Data.Text.Lazy.IO.putStrLn $ encodeToLazyText x)
  return x

configuredLights :: String -> HueHandler (Map Int Light)
configuredLights _ = askingState allHueLights

configuredGroups :: String -> HueHandler (Map Int Group)
configuredGroups _ = askingState allHueGroups

configuredGroup :: String -> Int ->HueHandler Group
configuredGroup _ 0 = askingState group0
configuredGroup _ _ = throwError err404

allConfig :: String -> HueHandler Everything
allConfig userId = do
  lights <- configuredLights userId
  groups <- configuredGroups userId
  config <- bridgeConfig userId
  let schedules = mempty
  let scenes = mempty
  let rules = mempty
  let sensors = mempty
  let resoucelinks = mempty
  return Everything {..}

hueApi :: Proxy (HueApi :<|> HueAPIV2.HueApiV2)
hueApi = Proxy

hueApp :: MVar ServerState -> Application
hueApp st = serve hueApi (hoistServer hueApi funToHandler hueServer)
  where funToHandler :: HueHandler a -> Handler a
        funToHandler f = runReaderT f st


mqttThread :: MVar ServerState -> IO ()
mqttThread st = mdo
  let (Just uri) = parseURI "mqtt://192.168.1.15" -- FIXME: take from server config
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback (msgReceived mc)} uri
  print =<< subscribe mc
                      [("homeassistant/light/+/light/config", subOptions {_subQoS = QoS1}),
                       ("zigbee2mqtt/+", subOptions {_subQoS = QoS1})]
                      -- TODO: read zigbee2mqtt/bridge/devices. Useful to get model_id, network_address, ieee_address
                      []
  waitForClient mc   -- wait for the the client to disconnect

  where
    msgReceived mc _ (unTopic -> topic) msg _properties = do
      print (topic <> ":")
      Data.ByteString.Lazy.Char8.putStrLn msg
      case (decode msg, decode msg) of
        (Just l,_) | "homeassistant/light" `Text.isPrefixOf` topic -> do
           Text.putStrLn "Got light config"
           modifyMVarMasked_ st (return . updateLightConfig l)
           let Just t =  mkTopic (state_topic l <> "/get") 
           publish mc t "{\"state\": \"\"}" False -- request light state now
        (_,Just l) -> do
          Text.putStrLn "Got light state"
          modifyMVarMasked_ st (return . updateLightState topic l)
        _ -> do Text.putStrLn "Unknown kind of message"
      withMVar st (print . lightIds)
