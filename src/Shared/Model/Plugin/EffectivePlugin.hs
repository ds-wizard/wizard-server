module Shared.Model.Plugin.EffectivePlugin where

import qualified Data.Aeson as A
import GHC.Generics

import Shared.Model.Plugin.Plugin

data EffectivePlugin = EffectivePlugin
  { plugin :: Plugin
  , enabled :: Bool
  , overridden :: Bool
  , values :: Maybe A.Value
  }
  deriving (Generic, Eq, Show)
