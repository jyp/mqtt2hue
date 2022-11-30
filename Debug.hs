{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug where

import Data.Text
import Data.Text.IO as Text
import Control.Monad (when)

data Protocol = MQTT | Hue | SSDP

data DebugClass = Input Protocol | Output Protocol | Button | State

classMsg :: DebugClass -> Text
classMsg = \case
  Input p -> "<<" <> protoMsg p
  Output p -> ">>" <> protoMsg p
  Button -> "[]"
  State -> "!!"

protoMsg :: Protocol -> Text
protoMsg = \case
  MQTT -> "Q"
  Hue -> "H"
  SSDP -> "D"

activeProtocol :: Protocol -> Bool
activeProtocol = \case
  Hue -> True
  _ -> False

activeClass :: DebugClass -> Bool
activeClass = \case
  Input p -> activeProtocol p
  Output p -> activeProtocol p
  Button -> True
  State -> False

debug :: DebugClass -> Text -> IO ()
debug cls msg = do
  when (activeClass cls) $
    Text.putStrLn (classMsg cls <> " " <> msg)
