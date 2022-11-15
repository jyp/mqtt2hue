{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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
module Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
-- import Data.ByteString (ByteString)
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Concurrent.MVar

import Prelude ()
import Prelude.Compat
import Servant

import Types
import HueAPI
import MQTTAPI

type HueHandler = ReaderT (MVar ServerState) Handler

hueServer :: ServerT HueApi HueHandler
hueServer =  createUser
        :<|> bridgePublicConfig
        :<|> allConfig
        :<|> bridgeConfig
        :<|> configuredLights
        :<|> configuredGroups

serverConfig :: ServerConfig
serverConfig = ServerConfig { mac = "90:61:ae:21:8f:6d"
                            ,ipaddress = "192.168.1.50"
                            ,netmask = "255.255.255.0"
                            ,gateway = "192.168.1.1"
                            }

bridgeConfig :: String -> HueHandler Config
bridgeConfig _userId = bridgePublicConfig

createUser :: CreateUser -> HueHandler [CreatedUser]
createUser CreateUser {..} = do
  liftIO (Text.putStrLn ("user-creation requested for " <> devicetype))
  return [CreatedUser $ UserName "83b7780291a6ceffbe0bd049104df"]

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
  ,replacesbridgeid = HueAPI.Null
  ,backup = Backup {status = HueAPI.Idle
                   ,errorcode = 0
                   }
  ,starterkitid = ""
  ,whitelist = []
  , ..
  } where ServerConfig {..} = serverConfig

blankLightState :: HueAPI.LightState
blankLightState = HueAPI.LightState { on = True
                                    , bri = 23
                                    -- , hue = 44
                                    -- , sat = 15
                                    , ct = Nothing
                                    , effect = None
                                    , xy = Nothing
                                    , alert = Select
                                    , colorMode = Nothing
                                    , mode = HomeAutomation
                                    , reachable = True}

lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {..} = HueAPI.LightState
  {on = state == ON
  ,bri = brightness
  -- ,hue = _ -- FIXME
  -- ,sat = _
  ,ct = color_temp
  ,effect = None
  ,xy = fmap (\(ColorXY x y) -> [x,y]) color
  ,alert = Select
  ,colorMode = (<$> color_mode) $ \case
      TemperatureMode -> CT
      XYMode -> XY
  ,mode = HomeAutomation
  ,reachable = True -- FIXME -- linkquality ?
  }

lightMqtt2Hue :: MQTTAPI.LightConfig -> MQTTAPI.LightState -> HueAPI.Light
lightMqtt2Hue (MQTTAPI.LightConfig {device = Device {name=productName,..},..}) lightState
  = Light {state = lightStateMqtt2Hue lightState 
          ,swUpdate = SwUpdate {state = NoUpdates
                               ,lastinstall = UTCTime (toEnum 0) (toEnum 0) -- FIXME
                               }
          ,_type = if XYMode `elem` supported_color_modes then
                     ExtendedColorLight else (if TemperatureMode `elem` supported_color_modes
                                              then TemperatureLight
                                              else DimmableLight)
          ,name = name
          ,modelId = model
          ,manufacturerName = manufacturer
          ,productName = productName
          ,capabilities = Capabilities
            {certified = False,
             control = if XYMode `elem` supported_color_modes then
                     FullColor Other Nothing -- FIXME: Get gamut from manufacturer+modelid
                       else (if TemperatureMode `elem` supported_color_modes
                              then case (min_mireds,max_mireds) of
                                     (Just mmin, Just mmax) -> Ct (CtValues mmin mmax)
                                     _ -> NoControl
                              else NoControl)}
          ,config = HueAPI.LightConfig {
                         archetype = "sultanbulb",
                         function = "functional",
                         direction = "omnidirectional",
                         startup = Startup {mode = Safety, configured = True}}
          ,uniqueid = unique_id
          ,swversion = sw_version
          }


-- >>> (encode exampleLights)
-- "{\"1\":{\"capabilities\":{\"certified\":false,\"control\":{\"ct\":{\"max\":450,\"min\":0},\"tag\":\"Ct\"}},\"config\":{\"archetype\":\"sultanbulb\",\"direction\":\"omnidirectional\",\"function\":\"functional\",\"startup\":{\"configured\":true,\"mode\":\"safety\"}},\"manufacturerName\":\"Signify\",\"modeLid\":\"LTW010\",\"name\":\"Hue ambiance lamp in my office\",\"productName\":\"Hue ambiance lamp\",\"state\":{\"alert\":[],\"bri\":23,\"colorMode\":\"ct\",\"ct\":15,\"effect\":[],\"hue\":44,\"mode\":\"homeautomation\",\"on\":true,\"reachable\":true,\"sat\":15,\"xy\":[12,34]},\"swUpdate\":{\"lastinstall\":\"1858-11-17T00:00:00Z\",\"state\":\"noupdates\"},\"swversion\":\"test\",\"type\":\"Color temperature light\",\"uniqueid\":\"test\"}}"


configuredLights :: String -> HueHandler (Map Int Light)
configuredLights _ = return $ mempty



configuredGroups :: String -> HueHandler (Map Int Group)
configuredGroups _ = return $ mempty

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

hueApi :: Proxy HueApi
hueApi = Proxy @HueApi

hueApp :: MVar ServerState -> Application
hueApp st = serve hueApi (hoistServer hueApi funToHandler hueServer)
  where funToHandler :: HueHandler a -> Handler a
        funToHandler f = runReaderT f st


blankServerState :: ServerState
blankServerState = ServerState mempty mempty mempty

data DeviceKind = KLight
data ServerState = ServerState {lights :: Map Text MQTTAPI.LightConfig -- map from uniqueid to config
                               ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state
                               ,lightIds :: Map Text Int -- map from uniqueid to simple id
                               }

mqttThread :: MVar ServerState -> IO ()
mqttThread st = do
  let (Just uri) = parseURI "mqtt://192.168.1.15" -- FIXME: take from server config
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback msgReceived} uri
  print =<< subscribe mc
                      [("homeassistant/light/+/light/config", subOptions {_subQoS = QoS1}),
                       ("zigbee2mqtt/+", subOptions {_subQoS = QoS1})]
                      []
  waitForClient mc   -- wait for the the client to disconnect

  where
    msgReceived _ (unTopic -> topic) msg _properties = do
      case (decode msg, decode msg) of
        (Just l,_) | "homeassistant/light" `Text.isPrefixOf` topic -> do
           let uid = unique_id l
           Text.putStrLn $ ("Got light config: " <> uid)
           modifyMVarMasked_ st $ \ServerState{..} -> return
             ServerState{lights=Data.Map.insert uid l lights
                         ,lightIds=if uid `member` lightIds
                                   then lightIds
                                   else insert uid (1+maximum (0:elems lightIds)) -- if empty, assign 1.
                                       lightIds
                         ,..}
        (_,Just l) -> do
          Text.putStrLn $ ("Got light state: " <> topic)
          modifyMVarMasked_ st $ \ServerState{..} -> return ServerState{lightStates=Data.Map.insert topic l lightStates,..}
        _ -> do Text.putStrLn ("Got unknown kind of message on topic " <> topic)
                print msg
      withMVar st (print . lightIds)


