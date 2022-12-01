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

module Logic.Automation where

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
import Logic.Common

handleSwitchState :: Text -> SwitchState -> AppState -> (AppState, [AgendaItem])
handleSwitchState t SwitchState { action = BtnAction OnBtn Press }
                    st0@AppState{groups,groupLastRecallScene}
  = fromMaybe (st0,[]) $ do
      dev <- getDeviceByFriendlyName (topicFriendlyName t) st0
      let gids = [ gid | ep <- Map.elems (endpoints dev)
                       , Binding cluster
                         (Target {_id = Just gid,_type = "group"}) <- bindings ep
                       , cluster == "genOnOff"]
      gid <- listToMaybe gids
      g <- Map.lookup gid groups
      MQTTAPI.LightState {state} <- getGroupState g st0
      -- return [Todo "hue2mqtt/debug/switchscene/group" g "",
      --         Todo "hue2mqtt/debug/switchscene/state" state ""]
      when (state  == OFF) $
        fail "don't change scene if group is off"
      let lastScene = Map.findWithDefault lastAvailableScene gid groupLastRecallScene
          gss = groupScenes st0 gid
          -- by default take the last scene in the list, so the activated scene will be the 1st
          lastAvailableScene:_ = reverse gss ++ [0]
      -- return [Todo "hue2mqtt/debug/switchscene/lastscene" lastScene ""
      --        ,Todo "hue2mqtt/debug/switchscene/lastAvailableScene" lastAvailableScene ""]
      case dropWhile (/= lastScene) (gss ++ gss) of -- copy in case its last
        (_:nextScene:_) -> do
          let a = blankAction {scene_recall = Just nextScene}
          return (applyGroupAction st0 g a, [mkGroupAction g a])
        _ -> fail "next scene not found"
handleSwitchState _ _ st = (st, [])

