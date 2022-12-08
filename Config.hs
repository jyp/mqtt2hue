{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import qualified Data.Text as Text
import Data.Text (Text)
import Types
import Data.Char


mkBridgeIdLower :: NetConfig -> Text
mkBridgeIdLower NetConfig {..} = mac1 <> "fffe" <> mac2
 where (mac1,mac2) = Text.splitAt 6 (Text.pack (macHex mac))

mkBridgeIdUpper :: NetConfig -> Text
mkBridgeIdUpper = Text.map toUpper .  mkBridgeIdUpper

-- BSB001 is Hue bridge 1st gen
-- BSB002 is Hue bridge 2st gen

mkModelId :: Text
mkModelId = "BSB002"

apiVersion :: Text
apiVersion = "1.53.0"
  -- ,apiversion = "1.45.0" -- last version not to support event stream


mkSoftwareVersion :: Text
mkSoftwareVersion = "1.53." <> softwareVersionMinor

softwareVersionMinor :: Text
softwareVersionMinor = "1953188020"
