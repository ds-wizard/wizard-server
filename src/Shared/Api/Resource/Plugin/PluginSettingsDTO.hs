module Shared.Api.Resource.Plugin.PluginSettingsDTO where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import GHC.Generics

data PluginSettingsListDTO = PluginSettingsListDTO
  { pluginUuid :: U.UUID
  , enabled :: Bool
  , workspaceOverrideAllowed :: Maybe Bool
  , overrideAllowed :: Maybe Bool
  , overridden :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

data PluginSettingsChangeDTO = PluginSettingsChangeDTO
  { enabled :: Bool
  , workspaceOverrideAllowed :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

data WorkspacePluginSettingsDTO = WorkspacePluginSettingsDTO
  { overridden :: Bool
  , overrideAllowed :: Bool
  , values :: A.Value
  }
  deriving (Show, Eq, Generic)
