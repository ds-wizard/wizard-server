module Shared.Service.Mcp.TenantContext.TenantContextMapper where

import qualified Data.Aeson as A
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings
import Shared.Model.User.RoleSimple
import Shared.Model.Workspace.Workspace

toTenantContextDTO :: UserDTO -> [String] -> Bool -> Maybe U.UUID -> [TenantContextWorkspaceDTO] -> Maybe A.Value -> TenantContextSettingsDTO -> TenantContextDTO
toTenantContextDTO user permissions multiWorkspace mWorkspaceUuid workspaces mUsage settings =
  TenantContextDTO
    { user = toUserDTO user
    , permissions = L.sort . L.nub $ permissions
    , multiWorkspace = multiWorkspace
    , workspaceUuid = mWorkspaceUuid
    , workspaces = workspaces
    , usage = mUsage
    , settings = settings
    }

toUserDTO :: UserDTO -> TenantContextUserDTO
toUserDTO user =
  TenantContextUserDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , role = user.role.name
    }

toWorkspaceDTO :: M.Map U.UUID RoleSimple -> Workspace -> TenantContextWorkspaceDTO
toWorkspaceDTO workspaceRoles workspace =
  TenantContextWorkspaceDTO
    { uuid = workspace.uuid
    , name = workspace.name
    , description = workspace.description
    , role = (.name) <$> M.lookup workspace.uuid workspaceRoles
    }

toSettingsDTO :: SettingsDashboardAndMenu -> SettingsProjects -> SettingsSupport -> SettingsSubmission -> TenantContextSettingsDTO
toSettingsDTO dashboardAndMenu projects support submission =
  TenantContextSettingsDTO
    { dashboardAndMenu = dashboardAndMenu
    , projects = projects
    , support = support
    , submission = SimpleFeature submission.enabled
    }
