{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Logic where

import MQTTAPI
import HueAPI
import Data.Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.LocalTime


blankServerState :: ServerState
blankServerState = ServerState mempty mempty mempty

data ServerState = ServerState {lights :: Map Text MQTTAPI.LightConfig -- map from uniqueid to config
                               ,lightStates :: Map Text MQTTAPI.LightState -- map from topic to state
                               ,lightIds :: Map Text Int -- map from uniqueid to simple id
                               }

lightStateMqtt2Hue :: MQTTAPI.LightState -> HueAPI.LightState
lightStateMqtt2Hue MQTTAPI.LightState {..} = HueAPI.LightState
  {on = state == ON
  ,bri = brightness
  -- ,hue = _ -- FIXME
  -- ,sat = _
  ,ct = color_temp
  ,effect = None
  ,xy = fmap (\(ColorXY x y) -> [x,y]) color
  ,alert = Select
  ,colorMode = (<$> color_mode) $ \case
      TemperatureMode -> CT
      XYMode -> XY
  ,mode = HomeAutomation
  ,reachable = True -- FIXME -- linkquality ?
  }

lightMqtt2Hue :: MQTTAPI.LightConfig -> MQTTAPI.LightState -> HueAPI.Light
lightMqtt2Hue (MQTTAPI.LightConfig {device = Device {name=productName,..},..}) lightState
  = Light {state = lightStateMqtt2Hue lightState 
          ,swUpdate = SwUpdate {state = NoUpdates
                               ,lastinstall = UTCTime (toEnum 0) (toEnum 0) -- FIXME
                               }
          ,_type = if XYMode `elem` supported_color_modes then
                     ExtendedColorLight else (if TemperatureMode `elem` supported_color_modes
                                              then TemperatureLight
                                              else DimmableLight)
          ,name = name
          ,modelId = model
          ,manufacturerName = manufacturer
          ,productName = productName
          ,capabilities = Capabilities
            {certified = False,
             control = if XYMode `elem` supported_color_modes then
                     FullColor Other Nothing -- FIXME: Get gamut from manufacturer+modelid
                       else (if TemperatureMode `elem` supported_color_modes
                              then case (min_mireds,max_mireds) of
                                     (Just mmin, Just mmax) -> Ct (CtValues mmin mmax)
                                     _ -> NoControl
                              else NoControl)}
          ,config = HueAPI.LightConfig {
                         archetype = "sultanbulb",
                         function = "functional",
                         direction = "omnidirectional",
                         startup = Startup {mode = Safety, configured = True}}
          ,uniqueid = unique_id
          ,swversion = sw_version
          }


blankLightState :: HueAPI.LightState
blankLightState = HueAPI.LightState { on = True
                                    , bri = 23
                                    -- , hue = 44
                                    -- , sat = 15
                                    , ct = Nothing
                                    , effect = None
                                    , xy = Nothing
                                    , alert = Select
                                    , colorMode = Nothing
                                    , mode = HomeAutomation
                                    , reachable = True}


updateLightConfig :: MQTTAPI.LightConfig -> ServerState -> ServerState
updateLightConfig l ServerState {..} =
  ServerState { lights = Data.Map.insert uid l lights
              , lightIds = if uid `member` lightIds then lightIds else insert uid (1 + maximum (0 : elems lightIds)) lightIds
              , ..}
  where uid = unique_id l

updateLightState :: Text -> MQTTAPI.LightState -> ServerState -> ServerState
updateLightState topic l ServerState {..} = ServerState {lightStates = Data.Map.insert topic l lightStates, ..}
