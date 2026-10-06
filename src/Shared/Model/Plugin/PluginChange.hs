module Shared.Model.Plugin.PluginChange where

import qualified Data.UUID as U

data PluginChange
  = PluginUpdated U.UUID Bool (Maybe Bool)
  | PluginSettingsUpdated U.UUID
  | PluginUpdatedInWorkspace U.UUID U.UUID Bool
  | PluginSettingsUpdatedInWorkspace U.UUID U.UUID
  | PluginSettingsResetInWorkspace U.UUID U.UUID
  deriving (Show, Eq)
