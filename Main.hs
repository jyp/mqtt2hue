{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Server

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import Data.Time.Clock
import Data.Time.LocalTime
import           Network.Wai.Logger       (withStdoutLogger)
import           Control.Concurrent                  (forkIO)
import           Network.Wai                         (Application, responseLBS)
import           Network.Wai.Handler.Warp            (defaultSettings, run,
                                                      setPort)
import           Network.Wai.Handler.WarpTLS         (runTLS, tlsSettings)
import Data.Yaml
import Control.Concurrent.MVar

import Logic
import Types

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
