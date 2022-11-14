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
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Map
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.MQTT.Types
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude ()
import Prelude.Compat
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import Types
import HueAPI
import MQTTAPI

server1 :: Server HueApi
server1 =    createUser
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

bridgeConfig :: String -> Handler Config
bridgeConfig _userId = bridgePublicConfig

createUser :: CreateUser -> Handler [CreatedUser]
createUser CreateUser {..} = do
  liftIO (putStrLn ("user-creation requested for " <> devicetype))
  return [CreatedUser $ UserName "83b7780291a6ceffbe0bd049104df"]

bridgePublicConfig :: Handler Config
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

exampleLights :: Map Int Light
exampleLights  = 
  Data.Map.fromList [(1,Light {state = HueAPI.LightState { on = True
                                         , bri = 23
                                         -- , hue = 44
                                         -- , sat = 15
                                         , ct = 15
                                         , effect = None
                                         , xy = [12, 34]
                                         , alert = Select
                                         , colorMode = CT
                                         , mode = HomeAutomation
                                         , reachable = True}
                     ,swUpdate = SwUpdate {state = NoUpdates
                                          ,lastinstall = UTCTime (toEnum 0) (toEnum 0)
                                          }
                     ,_type = TemperatureLight
                     ,name = "Hue ambiance lamp in my office"
                     ,modeLid = "LTW010" -- ?
                     ,manufacturerName = "Signify"
                     ,productName = "Hue ambiance lamp"
                     ,capabilities = Capabilities
                       { certified = False,
                         control = Ct {ct = CtValues {min = 0,max = 450}}
                       }
                     ,config = HueAPI.LightConfig {
                         archetype = "sultanbulb",
                         function = "functional",
                         direction = "omnidirectional",
                         startup = Startup {mode = Safety, configured = True}}
                     ,uniqueid = "test"
                     ,swversion = "test"})]

lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {..} = HueAPI.LightState
  {on = state == ON
  ,bri = brightness
  -- ,hue = _ -- FIXME
  -- ,sat = _
  ,ct = color_temp
  ,effect = None
  ,xy = case color of ColorXY x y -> [x,y]
  ,alert = Select
  ,colorMode = case color_mode of
      TemperatureMode -> CT
      XYMode -> XY
  ,mode = HomeAutomation
  ,reachable = True -- FIXME -- linkquality ?
  }

lightMqtt2Hue :: MQTTAPI.LightConfig -> MQTTAPI.LightState -> HueAPI.Light
lightMqtt2Hue (MQTTAPI.LightConfig {device = Device {name=productName,manufacturer,sw_version},..}) lightState
  = Light {state = lightStateMqtt2Hue lightState 
          ,swUpdate = SwUpdate {state = NoUpdates
                               ,lastinstall = UTCTime (toEnum 0) (toEnum 0) -- FIXME
                               }
          ,_type = if XYMode `elem` supported_color_modes then
                     ExtendedColorLight else (if TemperatureMode `elem` supported_color_modes
                                              then TemperatureLight
                                              else DimmableLight)
          ,name = name
          ,modeLid = "" -- ???
          ,manufacturerName = manufacturer
          ,productName = productName
          ,capabilities = Capabilities
            {certified = False,
             control = if XYMode `elem` supported_color_modes then
                     FullColor Other Nothing -- FIXME: Get gamut from manufacturer
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


configuredLights :: String -> Handler (Map Int Light)
configuredLights _ = return $ mempty



configuredGroups :: String -> Handler (Map Int Group)
configuredGroups _ = return $ mempty

allConfig :: String -> Handler Everything
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

app1 :: Application
app1 = serve (Proxy @HueApi) server1


mqttapp :: IO ()
mqttapp = do
  let (Just uri) = parseURI "mqtt://192.168.1.15"
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback msgReceived} uri
  -- let Just flt = mkFilter "#"
  -- let Just flt = mkFilter "homeassistant/switch/+/switch/config"
  let Just flt = mkFilter "homeassistant/light/+/light/config"
  putStrLn $ "subscribing to: " <> show flt
  print =<< subscribe mc [(flt, subOptions {_subQoS = QoS2})] []
  waitForClient mc   -- wait for the the client to disconnect

  where
    msgReceived _ topic msg properties = do
      let (m :: Maybe MQTTAPI.LightConfig) = decode msg
      print m



  -- let (Just uri) = parseURI "mqtt://192.168.1.15"
  -- client <- connectURI mqttConfig {_msgCB=SimpleCallback msgReceived} uri
  -- let Just flt = mkFilter "/homeassistant/light/+/light/config"
  -- putStrLn ("ready to subscribe to:" <> show flt)
  -- print =<< subscribe client [(flt, subOptions {_subQoS = QoS2})] []
  -- waitForClient client   -- wait for the the client to disconnect
  -- where
  --   msgReceived _ t m p = print (t,m,p)
