module Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings

data TenantContextDTO = TenantContextDTO
  { user :: TenantContextUserDTO
  , permissions :: [String]
  , multiWorkspace :: Bool
  , workspaceUuid :: Maybe U.UUID
  , workspaces :: [TenantContextWorkspaceDTO]
  , usage :: Maybe A.Value
  , settings :: TenantContextSettingsDTO
  }
  deriving (Show, Eq, Generic)

data TenantContextUserDTO = TenantContextUserDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , role :: String
  }
  deriving (Show, Eq, Generic)

data TenantContextWorkspaceDTO = TenantContextWorkspaceDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , role :: Maybe String
  }
  deriving (Show, Eq, Generic)

data TenantContextSettingsDTO = TenantContextSettingsDTO
  { dashboardAndMenu :: SettingsDashboardAndMenu
  , projects :: SettingsProjects
  , support :: SettingsSupport
  , submission :: SimpleFeature
  }
  deriving (Show, Eq, Generic)
