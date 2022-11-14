{-# LANGUAGE LambdaCase #-}
module MyAeson where

import Data.Aeson.TH
import Language.Haskell.TH.Syntax
import Data.Char

myDeriveToJSON :: Name -> Q [Dec]
myDeriveToJSON = deriveToJSON (defaultOptions {fieldLabelModifier = fmap toLower . \case ('_':rest) -> rest; x -> x, constructorTagModifier = fmap toLower})
