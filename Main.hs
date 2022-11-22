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
import System.Environment

main :: IO ()
main = do
  [cfgFileName] <- getArgs
  cfg <- decodeFileThrow cfgFileName
  st <- newMVar blankAppState
  mv <- newEmptyMVar
  button <- newEmptyMVar
  semas <- newMVar mempty
  mac <- Text.strip <$> Text.readFile ("/sys/class/net/" <> netInterface cfg <> "/address")
  let netCfg = NetConfig {..} where ServerConfig{..} = cfg
  db <- newMVar =<< decodeFileThrow (usersFilePath cfg) 
  let s = ServerState cfg netCfg st mv db button semas
  _ <- forkIO (mqttThread s)
  let app = hueApp s
  withStdoutLogger $ \aplogger -> do
    let tlsOpts = tlsSettings "certificate/cert.pem" "certificate/privkey.pem"
        warpOpts = setLogger aplogger $ defaultSettings
    _ <- forkIO $ runSettings (setPort 80 warpOpts) app
    runTLS tlsOpts (setPort 443 $ warpOpts) app
  return ()
