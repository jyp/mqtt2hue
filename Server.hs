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
import qualified Data.Text.Lazy.IO as Text.Lazy
import qualified Data.Text.IO as Text
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Servant.Types.SourceT as S
import Data.Text.Encoding
import Data.Hashable
import qualified Data.Yaml
import Data.Text (Text)

import Servant
import Logic
import Types
import HueAPI
import HueAPIV2
import MQTTAPI

data ServerState = ServerState {serverConfig :: ServerConfig
                               ,netConfig :: NetConfig
                               ,appState :: MVar AppState
                               ,mqttState :: MVar MQTTClient
                               ,database :: MVar DataBase
                               ,buttonPressed :: MVar Word128
                               }

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
        :<|> configuredScenes

hueServerV2 :: ServerT HueAPIV2.HueApiV2 HueHandler
hueServerV2 = return (S.fromStepT s) -- FIXME: broken.
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


bridgeConfig :: Text -> HueHandler Config
bridgeConfig userId = do
  verifyUser userId
  bridgePublicConfig

createUser :: CreateUser -> HueHandler [CreatedUser]
createUser CreateUser {devicetype=applicationIdentifier} = do
  liftIO (Text.putStrLn ("user-creation requested for " <> applicationIdentifier))
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

verifyUser :: Text -> HueHandler ()
verifyUser userId = do
  dbVar <- asks database
  users <- liftIO $ readMVar dbVar
  when (notElem userId (applicationKey <$> users)) $
    throwError err300


bridgePublicConfig :: HueHandler Config
bridgePublicConfig = do
 NetConfig {..} <- askConfig
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
    -- BSB001 is Hue bridge 1st gen
    -- BSB002 is Hue bridge 2st gen
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
  }

askApp :: HueHandler AppState
askApp = liftIO . readMVar . appState =<< ask

askConfig :: HueHandler NetConfig
askConfig = asks netConfig

askingState :: ToJSON a => (AppState -> a) -> HueHandler a
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
    

configuredLights :: Text -> HueHandler (Map Int Light)
configuredLights userId = do
  verifyUser userId
  askingState allHueLights

configuredGroups :: Text -> HueHandler (Map Int Group)
configuredGroups userId = do
  verifyUser userId
  askingState allHueGroups

configuredScenes :: Text -> HueHandler (Map Int Scene)
configuredScenes userId = do
  verifyUser userId
  askingState allHueScenes

configuredGroup :: Text -> Int -> HueHandler Group
configuredGroup userId i = do
  verifyUser userId
  withAppState (getHueGroup i)

groupAction :: Text -> Int -> HueAPI.Action -> HueHandler Text.Text
groupAction userId groupId a0 = do
  verifyUser userId
  liftIO $ Text.putStrLn ("[[[ " <> Text.pack (show groupId) <> " " <> Text.pack (show a0))
  as <- withAppState (handleGroupAction groupId a0)
  forM_ as $ \(t,a) -> do
    let Just t' = mkTopic t
    appPublish t' a
  return "Updated."

appPublish :: (ToJSON a) => Topic -> a -> HueHandler ()
appPublish t a = do
  liftIO (Text.Lazy.putStrLn (">>> " <> Text.Lazy.fromStrict (unTopic t) <> ": " <> encodeToLazyText a))
  mc <- liftIO . readMVar . mqttState =<< ask
  liftIO (publish mc t (encode a) False)

lightAction :: Text -> Int -> HueAPI.Action -> HueHandler Text.Text
lightAction userId lightId action = do
  verifyUser userId
  liftIO $ Text.putStrLn ("[[[ " <> Text.pack (show lightId) <> " " <> Text.pack (show action))
  (t,a) <- withAppState (Just . handleLightAction lightId action )
  let Just t' = mkTopic t
  appPublish t' a
  return "Updated."


allConfig :: Text -> HueHandler Everything
allConfig userId = do
  verifyUser userId
  config <- bridgePublicConfig
  e <- askingState (\st ->
      let lights = allHueLights st
          groups = allHueGroups st
          scenes = allHueScenes st
          schedules = mempty
          rules = mempty
          sensors = mempty
          resoucelinks = mempty
      in Everything {..})
  liftIO $ Text.Lazy.putStrLn ("]]] " <> encodeToLazyText e)
  return e

hueApi :: Proxy (HueApi :<|> HueAPIV2.HueApiV2)
hueApi = Proxy

hueApp :: ServerState -> Application
hueApp st = serve hueApi (hoistServer hueApi funToHandler hueServer)
  where funToHandler :: HueHandler a -> Handler a
        funToHandler f = runReaderT f st


mqttThread :: ServerState -> IO ()
mqttThread (ServerState serverConf@ServerConfig{..} _ st mv _ butMv) = mdo
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
      Text.putStrLn ("<<< " <> topic <> ": " <> decodeUtf8 msg')
      -- Data.ByteString.Lazy.Char8.putStrLn msg
      case () of
        () | topic == "mqtt2hue/pushbutton" -> do
             now <- getCurrentTime
             b <- tryPutMVar butMv (Word128 (fromIntegral (hash now)) (fromIntegral (hash serverConf)))
             if b
               then putStrLn "XXX Button pushed:" 
               else putStrLn "XXX Button stuck!"
             _ <- forkIO $ do
               threadDelay (1000*1000*30) -- 30 seconds
               c <- tryTakeMVar butMv
               case c of
                 Nothing -> putStrLn "XXX Button bounce"
                 Just _ -> putStrLn "XXX Button released"
             return ()
        () | "homeassistant/light" `Text.isPrefixOf` topic,
             Just l <- decode msg  -> do
           modifyMVarMasked_ st (return . updateLightConfig l)
           withMVar st $ \AppState{lights} -> Text.putStrLn ("!!!" <> Text.pack (show lights))
           let Just t =  mkTopic (state_topic l <> "/get") 
           publish mc t "{\"state\": \"\"}" False -- request light state now
        () | "zigbee2mqtt/" `Text.isPrefixOf` topic,
             Just (l::MQTTAPI.LightState) <- decode msg -> do
          modifyMVarMasked_ st (return . updateLightState topic l)
          withMVar st $ \AppState{lightStates} -> Text.putStrLn ("!!!" <> Text.pack (show lightStates))
        () | topic == "zigbee2mqtt/bridge/devices",
             Just ds <- decode msg -> do
          modifyMVarMasked_ st (\s -> return (s {zigDevices = fromList [(ieee_address,d) | d@ZigDevice{ieee_address} <- ds] }))
        () | topic == "zigbee2mqtt/bridge/groups",
             Just gs <- decode msg -> do
          modifyMVarMasked_ st (\s -> return (s {groups = fromList [(i,d) | d@MQTTAPI.GroupConfig{_id=i} <- gs] } :: AppState))
          withMVar st $ \AppState{groups} -> Text.putStrLn ("!!!" <> Text.pack (show groups))
        _ -> do Text.putStrLn "Unknown kind of message"

