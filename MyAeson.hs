{-# LANGUAGE LambdaCase #-}
module MyAeson where

import Data.Aeson.TH
import Language.Haskell.TH.Syntax
import Data.Char

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
