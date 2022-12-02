{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.EventStream where

import Servant
import qualified Network.HTTP.Media as M
import Data.Typeable
import Data.ByteString.Lazy
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Data.Aeson

-- imitate the Servant JSON and OctetStream implementations
data EventStream deriving Typeable

instance Accept EventStream where
  contentType _ = "text" M.// "event-stream"

instance MimeRender EventStream SSE where
  mimeRender _ = \case
    DataEv i x -> "id: " <> encodeUtf8 (Text.pack (show i)) <> ":0\n" <>
                  "data: "<> encode [x] <> "\n"
    HelloEv -> ": hi\n"

-- imitate the ToJSON type class
class ToSSE a where
  toSSE :: a -> ByteString

data SSE where
  HelloEv :: SSE
  DataEv :: ToJSON a => Int -> a -> SSE

-- my custom type with simple SSE render
data Hello = Hello

instance ToSSE Hello where
  toSSE _ = "hi:"
