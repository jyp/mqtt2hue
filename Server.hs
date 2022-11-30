{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ByteString.Lazy
import Data.Aeson.Text
import Data.Map
import Data.Time.Clock
import Data.Time.LocalTime
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI
import Network.Wai
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Servant.Types.SourceT as S
import Data.Text.Encoding
import Data.Hashable
import qualified Data.Yaml
import Data.Text (Text)

import Debug
import Servant
import Logic
import Types
import HueAPI
import qualified HueAPIV2 as V2
import MQTTAPI as MQTT
import Semaphores
import Config
import qualified Data.HashMap.Strict as H
import Xmlbf as X
import SSDP (getUUIDFromMacAddress)
data ServerState = ServerState {serverConfig :: ServerConfig
                               ,netConfig :: NetConfig
                               ,appState :: MVar AppState
                               ,mqttState :: MVar MQTTClient
                               ,database :: MVar DataBase
                               ,buttonPressed :: MVar Word128
                               ,serverSemaphores :: MultiSem Text
                               }

type HueHandler = ReaderT ServerState Handler

hueServerV1 :: ServerT HueApi HueHandler
hueServerV1 = getDescription
         :<|> createUser
         :<|> getBridgeConfig
         :<|> getConfig
         :<|> bridgeConfig
         :<|> putConfig
         :<|> getLights
         :<|> putLight
         :<|> getGroups
         :<|> getGroup
         :<|> putGroup
         :<|> getScenes

hueServerV2 :: ServerT V2.HueApiV2 HueHandler
hueServerV2 = eventStreamGet :<|> bridgeGet

okResponse1 :: a -> V2.Response a
okResponse1 x = V2.Response [x] []

bridgeGet :: Maybe Text -> ReaderT ServerState Handler (V2.Response V2.BridgeGet)
bridgeGet userId = do
  netCfg <- askConfig
  verifyUser2 userId
  return $ okResponse1 $ V2.BridgeGet
    {_id = _
    ,id_v1 = ""
    ,owner = V2.ResourceRef {rid = _
                            ,rtype = _
                            }
    ,bridge_id = mkBridgeId netCfg
    ,time_zone = V2.TimeZone "Europe/Stockholm" -- FIXME: config
    ,_type = V2.Bridge
    }
  

eventStreamGet :: Maybe Text -> ReaderT ServerState Handler (S.SourceT IO a)
eventStreamGet userId = do
  verifyUser2 userId
  return (S.fromStepT s) -- FIXME: broken.
       where s = S.Effect (threadDelay 1000000 >> return s)
  -- where s = S.Yield (V2.Event {resource = LightRes
  --                                   ,idv1 = _
  --                                   ,idv2 = _
  --                                   ,creationTime = _
  --                                   ,dimming = _
  --                                   ,color = _
  --                                   ,color_temperature = _
  --                                   }) _ 


getDescription :: ReaderT ServerState Handler [Node]
getDescription = do
  cfg <- asks netConfig
  return (Server.description cfg)
  
description :: NetConfig -> [Node]
description NetConfig{..} =
  element "root" (H.fromList [("xmlns","urn:schemas-upnp-org:device-1-0")]) (
  element "specVersion" mempty 
    (element "major" mempty (X.text "1")  <>
     element "minor" mempty (X.text "0")) <>
  element "URLBase" mempty (X.text ("http://" <> ip <> ":80/")) <>
  element "device" mempty dev)
 where
   dev = 
     element "deviceType" mempty (X.text "urn:schemas-upnp-org:device:Basic:1") <>
     element "friendlyName" mempty (X.text ("Philips hue (" <> ip <> ")")) <>
     element "manufacturer" mempty (X.text "Signify") <>
     element "manufacturerURL" mempty (X.text "http://www.philips-hue.com") <>
     element "modelDescription" mempty (X.text "Philips hue Personal Wireless Lighting") <>
     element "modelName" mempty (X.text "Philips hue bridge 2015") <>
     element "modelNumber" mempty (X.text "BSB002") <>
     element "modelURL" mempty (X.text "http://www.philips-hue.com") <>
     element "serialNumber" mempty (X.text (Text.Lazy.pack (macHex mac))) <>
     element "UDN" mempty (X.text "uuid:" <> X.text (fs (getUUIDFromMacAddress mac))) <>
     element "presentationURL" mempty (X.text "index.html") <>
     element "iconList" mempty icon
   icon =
     element "icon" mempty (
       element "mimetype" mempty (X.text "image/png") <>
       element "height" mempty (X.text "48") <>
       element "width" mempty (X.text "48") <>
       element "depth" mempty (X.text "24") <>
       element "url" mempty (X.text "hue_logo_0.png"))
   ip = fs ipaddress
   fs = Text.Lazy.fromStrict

hueServer :: ServerT (HueApi :<|> V2.HueApiV2) HueHandler
hueServer = hueServerV1 :<|> hueServerV2


bridgeConfig :: Text -> HueHandler Config
bridgeConfig userId = do
  verifyUser userId
  getBridgeConfig

putConfig :: Text -> Value -> HueHandler Text
putConfig userId _ = do
  verifyUser userId
  -- A big lie. But this is configured by other means.
  return "Updated."

createUser :: CreateUser -> HueHandler [CreatedUser]
createUser CreateUser {devicetype=applicationIdentifier} = do
  liftIO (debug (Input Hue) ("user-creation requested for " <> applicationIdentifier))
  but <- asks buttonPressed
  dbVar <- asks database
  dbFname <- asks (usersFilePath . serverConfig)
  applicationKey <- liftIO $ (Text.pack . show <$> readMVar but)
  creationDate <- liftIO getCurrentTime
  liftIO $ modifyMVarMasked_ dbVar $
    \users -> do
      let db = UserEntry{..}:users
      Data.Yaml.encodeFile dbFname db
      return db
  return [CreatedUser $ UserName applicationKey]

verifyUser2 :: Maybe Text -> HueHandler ()
verifyUser2 = \case
  Just u -> verifyUser u
  Nothing -> throwError err300

verifyUser :: Text -> HueHandler ()
verifyUser userId = do
  dbVar <- asks database
  users <- liftIO $ readMVar dbVar
  when (notElem userId (applicationKey <$> users)) $
    throwError err300


getBridgeConfig :: HueHandler Config
getBridgeConfig = do
 netCfg@NetConfig {..} <- askConfig
 now <- liftIO getCurrentTime
 return $ Config
  {name = "MQTT2hue" -- "Philips Hue"
  ,zigbeechannel = 15
  ,bridgeid = mkBridgeId netCfg
  ,dhcp = True
  ,proxyaddress = "none"
  ,proxyport = 0
  ,_UTC = now
  ,localtime = now
  ,timezone = "Europe/Stockholm"
  ,modelid = mkModelId
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
  ,mac=Text.pack (macHex mac)
  ,..
  }

askApp :: HueHandler AppState
askApp = liftIO . readMVar . appState =<< ask

askConfig :: HueHandler NetConfig
askConfig = asks netConfig

askingState :: (AppState -> a) -> HueHandler a
askingState f = do
  st <- askApp
  let x = f st
  -- liftIO (Data.Text.Lazy.IO.putStrLn $ encodeToLazyText x)
  return x

withAppState :: (AppState -> Maybe (AppState, a)) -> HueHandler a
withAppState f = do
  mv <- asks appState
  x <- liftIO $ modifyMVarMasked mv $ \st -> return $ 
    case f st of
      Nothing -> (st,Nothing)
      Just (st',a) -> (st',Just a)
  case x of
    Nothing -> throwError err400
    Just a -> return a
    -- putStrLn $ "!!! " <> show st'
    

getLights :: Text -> HueHandler (Map Int Light)
getLights userId = do
  verifyUser userId
  askingState allHueLights

getGroups :: Text -> HueHandler (Map Int Group)
getGroups userId = do
  verifyUser userId
  askingState allHueGroups

getScenes :: Text -> HueHandler (Map Int Scene)
getScenes userId = do
  verifyUser userId
  askingState allHueScenes

getGroup :: Text -> Int -> HueHandler Group
getGroup userId i = do
  verifyUser userId
  withAppState (getHueGroup i)

runTodos :: [Todo] -> HueHandler Text.Text
runTodos as =
  do semas <- asks serverSemaphores
     mc <- liftIO . readMVar . mqttState =<< ask
     liftIO $ runTodos' mc semas as
     return "Updated."
     
runTodos' :: MQTTClient -> MultiSem Text -> [Todo] -> IO ()
runTodos' mc semas as = do
       _ <- forkIO $ forM_ as $ \case 
         (Todo t a _) -> do
           let Just t' = mkTopic t
           appPublish' mc t' a
       waitForSemaphoresAtMost 1000000 [n | Todo _ _ n <- as] semas
       -- the client will ask for states immediately upon recieving
       -- response. But we have not gotten the update from mqtt yet.
       -- So try wait for such updates before returning, but wait 1sec at most

putGroup :: Text -> Int -> HueAPI.Action -> HueHandler Text.Text
putGroup userId groupId a0 = do
  verifyUser userId
  liftIO $ debug (Input Hue) (Text.pack (show groupId) <> " " <> Text.pack (show a0))
  as <- withAppState (translateGroupAction groupId a0)
  runTodos as

appPublish :: (ToJSON a) => Topic -> a -> HueHandler ()
appPublish t a = do
  mc <- liftIO . readMVar . mqttState =<< ask
  liftIO (appPublish' mc t a)

appPublish' :: (ToJSON a) => MQTTClient -> Topic -> a -> IO ()
appPublish' mc t a = do
  debug (Output MQTT) (unTopic t <> ": " <> Text.Lazy.toStrict (encodeToLazyText a))
  publish mc t (Data.Aeson.encode a) False

putLight :: Text -> Int -> HueAPI.Action -> HueHandler Text.Text
putLight userId lightId action = do
  verifyUser userId
  liftIO $ debug (Input Hue) (Text.pack (show lightId) <> " " <> Text.pack (show action))
  a <- askingState (translateLightAction lightId action)
  runTodos [a]


getConfig :: Text -> HueHandler Everything
getConfig userId = do
  verifyUser userId
  config <- getBridgeConfig
  e <- askingState (\st ->
      let lights = allHueLights st
          groups = allHueGroups st
          scenes = allHueScenes st
          schedules = mempty
          rules = mempty
          sensors = mempty
          resoucelinks = mempty
      in Everything {..})
  liftIO $ debug (Output Hue) (Text.Lazy.toStrict (encodeToLazyText e))
  return e

hueApi :: Proxy (HueApi :<|> V2.HueApiV2)
hueApi = Proxy

instance ToXml [Node] where
  toXml = id


hueApp :: ServerState -> Application
hueApp st = serve hueApi (hoistServer hueApi funToHandler hueServer)
  where funToHandler :: HueHandler a -> Handler a
        funToHandler f = runReaderT f st


mqttThread :: ServerState -> IO ()
mqttThread (ServerState serverConf@ServerConfig{..} _ st mv _ butMv semas) = mdo
  let (Just uri) = parseURI mqttBroker
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback (msgReceived mc)} uri
  putMVar mv mc
  let opts = opts 
  print =<< subscribe mc ((,subOptions {_subQoS = QoS1}) <$>
                          ["homeassistant/light/+/light/config",
                           "zigbee2mqtt/bridge/devices",
                           "zigbee2mqtt/bridge/groups",
                           "zigbee2mqtt/+",
                           "mqtt2hue/pushbutton"])
                      []
  waitForClient mc   -- wait for the the client to disconnect

  where
    msgReceived mc _ (unTopic -> topic) msg _properties = do
      let msg' = Data.ByteString.Lazy.toStrict msg
      debug (Input MQTT) (topic <> ": " <> decodeUtf8 msg')
      -- Data.ByteString.Lazy.Char8.putStrLn msg
      case () of
        () | topic == "mqtt2hue/pushbutton" -> do
             now <- getCurrentTime
             b <- tryPutMVar butMv (Word128 (fromIntegral (hash now)) (fromIntegral (hash serverConf)))
             debug Button (if b then "Pushed" else "Stuck!")
             _ <- forkIO $ do
               threadDelay (1000*1000*30) -- 30 seconds
               c <- tryTakeMVar butMv
               debug Button $ case c of
                 Nothing -> "Bounce"
                 Just _ -> "Released"
             return ()
        () | "homeassistant/light" `Text.isPrefixOf` topic,
             Just l <- decode msg  -> do
           modifyMVarMasked_ st (return . updateLightConfig l)
           withMVar st $ \AppState{lights} -> debug State (Text.pack (show lights))
           let Just t =  mkTopic (state_topic l <> "/get") 
           publish mc t "{\"state\": \"\"}" False -- request light state now
        () | "zigbee2mqtt/" `Text.isPrefixOf` topic,
             Just (l::MQTT.LightState) <- decode msg -> do
          modifyMVarMasked_ st (return . updateLightState topic l)
          withMVar st $ \AppState{lightStates} -> debug State (Text.pack (show lightStates))
          signalSemaphore topic semas
        () | "zigbee2mqtt/" `Text.isPrefixOf` topic,
             Just (sw::MQTT.SwitchState) <- decode msg -> do
          msgs <- modifyMVarMasked st $ \s -> return (handleSwitchState topic sw s)
          runTodos' mc semas msgs
        () | topic == "zigbee2mqtt/bridge/devices",
             Just ds <- decode msg -> do
          modifyMVarMasked_ st (\s -> return (s {zigDevices = fromList [(ieee_address,d) | d@ZigDevice{ieee_address} <- ds] }))
          withMVar st $ \AppState{zigDevices} -> debug State (Text.pack (show zigDevices))
        () | topic == "zigbee2mqtt/bridge/groups",
             Just gs <- decode msg -> do
          modifyMVarMasked_ st (\s -> return (s {groups = fromList [(i,d) | d@MQTT.GroupConfig{_id=i} <- gs] } :: AppState))
          withMVar st $ \AppState{groups} -> debug State (Text.pack (show groups))
        _ -> do debug (Input MQTT) "Unknown kind of message"

