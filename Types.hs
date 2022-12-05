{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types (module Types, Word128(..),LargeHashable) where



import Prelude
import Data.Text
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import Numeric (showHex)
import Data.Hashable
import Network.Info             (MAC(..))
import Text.Printf
import Data.LargeHashable

data ServerConfig = ServerConfig { netInterface :: String,
                                   usersFilePath :: String,
                                   mqttBroker :: String,
                                   certificatePath :: String,
                                   timezone,
                                   netmask,
                                   gateway :: Text } deriving Generic
instance LargeHashable ServerConfig
instance FromJSON ServerConfig
data NetConfig = NetConfig { mac :: MAC,
                             ipaddress,
                             netmask,
                             timezone,
                             gateway :: Text } deriving (Generic, Show)

macContents (MAC a b c d e f) = [a,b,c,d,e,f]

data UserEntry = UserEntry
  {applicationKey :: Text
  ,applicationIdentifier :: Text
  ,creationDate :: UTCTime} deriving Generic
instance ToJSON UserEntry
instance FromJSON UserEntry

type DataBase = [UserEntry]

-- data Word128 = Word128 !Word64 !Word64 deriving (Eq,Ord)


show128 :: Word128 -> [Char]
show128 (Word128 a b) = s a <> s b
    where s x = pad (showHex x [])
          pad = Prelude.reverse . Prelude.take 16 .  (++ repeat '0') . Prelude.reverse

-- >>> show128 (Word128 1252349 210394)
-- "0000000000131bfd00000000000335da"

instance Hashable ServerConfig

macHex :: MAC -> String
macHex (MAC a b c d e f) = printf "%02x%02x%02x%02x%02x%02x" a b c d e f

hash128 :: LargeHashable a => a -> Word128
hash128 = unMD5Hash . largeHash md5HashAlgorithm
