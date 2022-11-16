{-# LANGUAGE FlexibleContexts #-}
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
import Network.URI
import Network.Wai
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
import HueAPIV2
import MQTTAPI

data ServerState = ServerState {appState :: MVar AppState
                               ,mqttState :: MVar MQTTClient}

type HueHandler = ReaderT ServerState Handler

hueServerV1 :: ServerT HueApi HueHandler
hueServerV1 =  createUser
        :<|> bridgePublicConfig
        :<|> allConfig
        :<|> bridgeConfig
        :<|> configuredLights
        :<|> lightAction
        :<|> configuredGroups
        :<|> configuredGroup
        :<|> groupAction

hueServerV2 :: ServerT HueAPIV2.HueApiV2 HueHandler
hueServerV2 = return (S.fromStepT s)
       where s = S.Effect (threadDelay 1000000 >> return s)
  -- where s = S.Yield (HueAPIV2.Event {resource = LightRes
  --                                   ,idv1 = _
  --                                   ,idv2 = _
  --                                   ,creationTime = _
  --                                   ,dimming = _
  --                                   ,color = _
  --                                   ,color_temperature = _
  --                                   }) _ 

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
  -- ,apiversion = "1.53.0"
  ,apiversion = "1.45.0" -- last version not to support event stream
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

askApp :: HueHandler AppState
askApp = liftIO . readMVar . appState =<< ask

askingState :: ToJSON a => (AppState -> a) -> HueHandler a
askingState f = do
  st <- askApp
  let x = f st
  liftIO (Data.Text.Lazy.IO.putStrLn $ encodeToLazyText x)
  return x

configuredLights :: String -> HueHandler (Map Int Light)
configuredLights _ = askingState allHueLights

configuredGroups :: String -> HueHandler (Map Int Group)
configuredGroups _ = askingState allHueGroups

configuredGroup :: String -> Int -> HueHandler Group
configuredGroup _ 0 = askingState group0
configuredGroup _ _ = throwError err404

groupAction :: String -> Int -> HueAPI.Action -> HueHandler Text.Text
groupAction _userId groupId action = do
  mc <- liftIO . readMVar . mqttState =<< ask
  error "groupAction: todo"

appPublish :: (ToJSON a) => Topic -> a -> HueHandler ()
appPublish t a = do
  liftIO (Text.putStrLn ("Publish on " <> unTopic t <> "..."))
  liftIO (Data.Text.Lazy.IO.putStrLn $ encodeToLazyText a)
  mc <- liftIO . readMVar . mqttState =<< ask
  liftIO (publish mc t (encode a) False)

lightAction :: String -> Int -> HueAPI.Action -> HueHandler Text.Text
lightAction _userId lightId action = do
  st <- askApp
  let Just t = mkTopic (state_topic (hueSmallIdToLightConfig st lightId) <> "/set") 
  appPublish t (convertAction action)
  return "Updated."


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

hueApp :: ServerState -> Application
hueApp st = serve hueApi (hoistServer hueApi funToHandler hueServer)
  where funToHandler :: HueHandler a -> Handler a
        funToHandler f = runReaderT f st


mqttThread :: ServerConfig -> ServerState -> IO ()
mqttThread ServerConfig {..} (ServerState st mv) = mdo
  let (Just uri) = parseURI ("mqtt://" <> ipaddress) 
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback (msgReceived mc)} uri
  putMVar mv mc
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

