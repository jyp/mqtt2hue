{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Server

import           Network.Wai.Logger       (withStdoutLogger)
import           Control.Concurrent                  (forkIO)
import           Network.Wai.Handler.Warp            (defaultSettings, setPort,setLogger,runSettings)
import           Network.Wai.Handler.WarpTLS         (runTLS, tlsSettings)
import Data.Yaml
import Control.Concurrent.MVar

import Logic
import Types

-- >>> readFile "/sys/class/net/wlp3s0/address"
-- "90:61:ae:21:8f:6d\n"

main :: IO ()
main = do
  st <- newMVar blankAppState
  mv <- newEmptyMVar
  button <- newEmptyMVar
  let cfg = ServerConfig { mac = "90:61:ae:21:8f:6d"
                         , ipaddress = "192.168.1.50"
                         , mqttBroker = "mqtt://huey:qwftyrsi@192.168.1.15"
                         , netmask = "255.255.255.0"
                         , usersFilePath = "users.yaml"
                         , gateway = "192.168.1.1"
                          }
  db <- newMVar =<< decodeFileThrow (usersFilePath cfg) 
  let s = ServerState cfg st mv db button
  _ <- forkIO (mqttThread s)
  let app = hueApp s
  withStdoutLogger $ \aplogger -> do
    let tlsOpts = tlsSettings "certificate/cert.pem" "certificate/privkey.pem"
        warpOpts = setLogger aplogger $ defaultSettings
    _ <- forkIO $ runSettings (setPort 80 warpOpts) app
    runTLS tlsOpts (setPort 443 $ warpOpts) app
  return ()
