{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module MyAeson where

import Data.Aeson.TH
import Data.Aeson
import Data.Aeson.Types
import Language.Haskell.TH.Syntax
import Data.Char
import Control.Applicative
import qualified Data.ByteString.Lazy
import GHC.Generics

options :: Options
options =
  defaultOptions
    {omitNothingFields=True
    ,fieldLabelModifier = \case ('_':rest) -> rest; x -> x
    ,constructorTagModifier = fmap toLower}

  
myDeriveToJSON :: Name -> Q [Dec]
myDeriveToJSON = deriveToJSON options

myDeriveFromJSON :: Name -> Q [Dec]
myDeriveFromJSON = deriveFromJSON options

myDeriveJSON :: Name -> Q [Dec]
myDeriveJSON = deriveJSON options

myGenericToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
myGenericToJSON = genericToJSON options

myGenericParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
myGenericParseJSON = genericParseJSON options

data Choice a b = Opt1 a | Opt2 b deriving Show

instance (FromJSON a, FromJSON b) => FromJSON (Choice a b) where
  parseJSON v = (Opt1 <$> parseJSON v) <|> (Opt2 <$> parseJSON v)

instance (ToJSON a, ToJSON b) => ToJSON (Choice a b) where
  toJSON = \case
    Opt1 x -> toJSON x
    Opt2 x -> toJSON x


decodeTestFile :: FromJSON a => String -> IO (Maybe a)
decodeTestFile fname = decode <$> Data.ByteString.Lazy.readFile fname
