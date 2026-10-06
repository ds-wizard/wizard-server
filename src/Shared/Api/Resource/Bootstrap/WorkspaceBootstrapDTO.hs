module Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings

data WorkspaceBootstrapDTO = WorkspaceBootstrapDTO
  { logo :: Maybe String
  , primaryColor :: Maybe String
  , dashboardAndMenu :: SettingsDashboardAndMenu
  , projects :: SettingsProjects
  , support :: SettingsSupport
  , submission :: SimpleFeature
  , disabledPlugins :: [U.UUID]
  , pluginSettings :: M.Map U.UUID A.Value
  }
  deriving (Generic, Eq, Show)
