{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Logic ( AppState(..), Todo(..),
              --  hue side
              allHueLights, allHueGroups, getHueGroup, allHueScenes,
              translateGroupAction, translateLightAction, 
              -- MQTT side
              blankAppState,  updateLightConfig, updateLightState,
              -- Automations
              handleSwitchState
             ) where
import Data.Maybe
import MQTTAPI
import HueAPI
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text,splitOn,unpack,pack,isInfixOf,toCaseFold)
import Data.Time.Clock
import Text.Read (readMaybe)
import MyAeson(Choice(..))
import Data.Aeson (ToJSON(..))
import qualified Data.List as List
import qualified Data.Text as Text
import Control.Monad

blankAppState :: AppState 
blankAppState = AppState mempty mempty mempty mempty mempty mempty mempty

data AgendaItem = forall a. ToJSON a => AgendaItem
  { outTopic :: Text
  , message :: a
  , notifyTopic :: Text }
