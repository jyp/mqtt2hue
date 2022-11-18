{-# LANGUAGE LambdaCase #-}
module MyAeson where

import Data.Aeson.TH
import Data.Aeson
import Language.Haskell.TH.Syntax
import Data.Char
import Control.Applicative

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


data Choice a b = Opt1 a | Opt2 b deriving Show

instance (FromJSON a, FromJSON b) => FromJSON (Choice a b) where
  parseJSON v = (Opt1 <$> parseJSON v) <|> (Opt2 <$> parseJSON v)
