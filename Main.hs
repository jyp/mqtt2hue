{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Server

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.IntMap
import Data.Time.Clock
import Data.Time.LocalTime
import Types
import Network.Wai.Handler.WarpTLS
import           Network.Wai.Logger       (withStdoutLogger)
import           Control.Concurrent                  (forkIO)
import           Network.HTTP.Types                  (status200)
import           Network.Wai                         (Application, responseLBS)
import           Network.Wai.Handler.Warp            (defaultSettings, run,
                                                      setPort)
import           Network.Wai.Handler.WarpTLS         (runTLS, tlsSettings)
import           Network.Wai.Middleware.EnforceHTTPS (EnforceHTTPSConfig (..))

import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

app = EnforceHTTPS.withConfig httpsConf app1

httpsConf :: EnforceHTTPSConfig
httpsConf = EnforceHTTPS.defaultConfig { httpsPort = 443 }


main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  let tlsOpts = tlsSettings "certificate/cert.pem" "certificate/privkey.pem"
      warpOpts = setLogger aplogger $ defaultSettings
  _ <- forkIO $ runSettings (setPort 80 warpOpts) app1
  runTLS tlsOpts (setPort 443 $ warpOpts) app1
