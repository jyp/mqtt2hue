{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Server

import           Network.Wai.Logger       (withStdoutLogger)
import           Control.Concurrent                  (forkIO)
import           Network.Wai.Handler.Warp            (defaultSettings, setPort,setLogger,runSettings)
import           Network.Wai.Handler.WarpTLS         (runTLS, tlsSettings)
import Data.Yaml
import Data.Text.IO as Text
import Data.Text as Text
import Control.Concurrent.MVar

import Logic
import Types
import Config
import System.Environment
import SSDP
import Network.Info
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- >>> getNetworkInterfaces
-- [NetworkInterface {name = "lo", ipv4 = 127.0.0.1, ipv6 = 0:0:0:0:0:0:0:1, mac = 00:00:00:00:00:00},NetworkInterface {name = "enp2s0", ipv4 = 0.0.0.0, ipv6 = 0:0:0:0:0:0:0:0, mac = 30:9c:23:45:56:bc},NetworkInterface {name = "wlp3s0", ipv4 = 192.168.1.20, ipv6 = fe80:0:0:0:a576:416d:17d1:8d22, mac = 90:61:ae:21:8f:6d}]


main :: IO ()
main = do
  [cfgFileName] <- getArgs
  cfg@ServerConfig{netInterface} <- decodeFileThrow cfgFileName
  interfaces <- Prelude.filter (\NetworkInterface{name} -> name == netInterface) <$>
                getNetworkInterfaces
  case interfaces of
    [] -> error ("No network interface matches" <> netInterface)
    (itf:_) -> configured cfg itf

configured :: ServerConfig -> NetworkInterface -> IO ()
configured cfg@ServerConfig{certificatePath} itf = do
  Text.putStrLn "Configured"
  mv <- newEmptyMVar
  button <- newEmptyMVar
  semas <- newMVar mempty

  let hueService = SSDPService {
        ssOSName         = "Hue/1.0",
        ssInterface      = itf,
        ssPort           = 80,
        ssURI            = "/description.xml",
        ssProductName    = "IpBridge",
        ssProductVersion = apiVersion, -- check consistency with apiversion in Hue API. FIXME
        ssServiceItems   = ["urn:schemas-upnp-org:device:basic:1"]
        }
  ssdpHandle <- startSsdpServer hueService
                   
  let netCfg =
        NetConfig {
        ipaddress=pack (show ipv4),
        ..} where ServerConfig{..} = cfg; NetworkInterface{..}=itf

  st <- newMVar (blankAppState netCfg)

  Text.putStrLn "Loading user file"
  db <- newMVar =<< decodeFileThrow (usersFilePath cfg) 
  Text.putStrLn "Loaded"
  let s = ServerState cfg netCfg st mv db button semas
  _ <- forkIO (mqttThread s)
  Text.putStrLn "MQTT thread running"
  let app = logStdoutDev (hueApp s)
  withStdoutLogger $ \aplogger -> do
    let tlsOpts = tlsSettings (certificatePath <> "/cert.pem") (certificatePath <> "/privkey.pem")
        warpOpts = setLogger aplogger $ defaultSettings
    _ <- forkIO $ runSettings (setPort (httpPort cfg) warpOpts) app
    runTLS tlsOpts (setPort (httpsPort cfg) $ warpOpts) app
  return ()
