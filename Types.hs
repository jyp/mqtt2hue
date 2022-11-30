{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Prelude
import Data.Text
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import Numeric (showHex)
import Data.Hashable
import Data.Word
import Network.Info             (MAC(..))
import Text.Printf

data ServerConfig = ServerConfig { netInterface :: String,
                                   usersFilePath :: String,
                                   mqttBroker :: String,
                                   certificatePath :: String,
                                   netmask,
                                   gateway :: Text } deriving Generic

instance FromJSON ServerConfig
data NetConfig = NetConfig { mac :: MAC,
                             ipaddress,
                             netmask,
                             gateway :: Text } deriving Generic


data UserEntry = UserEntry
  {applicationKey :: Text
  ,applicationIdentifier :: Text
  ,creationDate :: UTCTime} deriving Generic
instance ToJSON UserEntry
instance FromJSON UserEntry

type DataBase = [UserEntry]

data Word128 = Word128 Word64 Word64 deriving (Eq,Ord)

instance Show Word128 where
  show (Word128 a b) = s a <> s b
    where s x = pad (showHex x [])
          pad = Prelude.reverse . Prelude.take 16 .  (++ repeat '0') . Prelude.reverse

-- >>> show (Word128 0 0)
-- "00000000000000000000000000000000"

instance Hashable ServerConfig

macHex :: MAC -> String
macHex (MAC a b c d e f) = printf "%02x%02x%02x%02x%02x%02x" a b c d e f
