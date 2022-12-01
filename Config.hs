{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import qualified Data.Text as Text
import Data.Text (Text)
import Types
import Data.Char


mkBridgeIdUpper :: NetConfig -> Text
mkBridgeIdUpper NetConfig {..} = mac1 <> "FFFE" <> mac2
 where (mac1,mac2) = Text.splitAt 6 (Text.pack (macHex mac))

mkBridgeIdLower :: NetConfig -> Text
mkBridgeIdLower = Text.map toLower .  mkBridgeIdUpper

-- BSB001 is Hue bridge 1st gen
-- BSB002 is Hue bridge 2st gen

mkModelId :: Text
mkModelId = "BSB002"


