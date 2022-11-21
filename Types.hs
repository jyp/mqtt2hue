{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Prelude ()
import Prelude.Compat
import Data.Text
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import Numeric (showHex)
import Data.Hashable
import Data.Word

data ServerConfig = ServerConfig { netInterface :: String,
                                   usersFilePath :: String,
                                   mqttBroker :: String,
                                   ipaddress,
                                   netmask,
                                   gateway :: Text } deriving Generic

instance FromJSON ServerConfig
data NetConfig = NetConfig { mac,
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

data Word128 = Word128 Word64 Word64

instance Show Word128 where
  showsPrec _ (Word128 a b) = showHex a . showHex b

instance Hashable ServerConfig
