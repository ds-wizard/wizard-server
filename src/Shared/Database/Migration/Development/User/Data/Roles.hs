module Shared.Database.Migration.Development.User.Data.Roles where

import qualified Data.UUID as U

import Shared.Constant.Workspace
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Util.Date
import Shared.Util.Uuid

adminRole :: Role
adminRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000001"
    , name = "Admin"
    , permissions = allRolePermissions
    , isAdmin = True
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

dataStewardRole :: Role
dataStewardRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000002"
    , name = "Data Steward"
    , permissions =
        [ _PROJECTS_CREATE_ROLE_PERMISSION
        , _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
        , _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
        , _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
        , _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
        , _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
        ]
    , isAdmin = False
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

researcherRole :: Role
researcherRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000003"
    , name = "Researcher"
    , permissions = [_PROJECTS_CREATE_ROLE_PERMISSION]
    , isAdmin = False
    , tenantUuid = defaultTenant.uuid
    , workspaceUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

deletableRole :: Role
deletableRole =
  researcherRole
    { uuid = u' "a0000000-0000-0000-0000-0000000000ff"
    , name = "Temporary Role"
    , permissions = []
    , isAdmin = False
    }

workspaceAdminRole :: U.UUID -> U.UUID -> U.UUID -> Role
workspaceAdminRole uuid tenantUuid workspaceUuid =
  Role
    { uuid = uuid
    , name = "Admin"
    , permissions = workspaceRolePermissions
    , isAdmin = False
    , tenantUuid = tenantUuid
    , workspaceUuid = Just workspaceUuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

workspaceUserRole :: U.UUID -> U.UUID -> U.UUID -> Role
workspaceUserRole uuid tenantUuid workspaceUuid = (workspaceAdminRole uuid tenantUuid workspaceUuid) {name = "User", permissions = []}

defaultWorkspaceAdminRole :: Role
defaultWorkspaceAdminRole = workspaceAdminRole (u' "a0000000-0000-0000-0000-000000000021") defaultTenant.uuid defaultWorkspaceUuid

defaultWorkspaceUserRole :: Role
defaultWorkspaceUserRole = workspaceUserRole (u' "a0000000-0000-0000-0000-000000000022") defaultTenant.uuid defaultWorkspaceUuid

secondWorkspaceAdminRole :: Role
secondWorkspaceAdminRole = workspaceAdminRole (u' "a0000000-0000-0000-0000-000000000031") defaultTenant.uuid secondWorkspaceUuid

secondWorkspaceUserRole :: Role
secondWorkspaceUserRole = workspaceUserRole (u' "a0000000-0000-0000-0000-000000000032") defaultTenant.uuid secondWorkspaceUuid

differentWorkspaceAdminRole :: Role
differentWorkspaceAdminRole = workspaceAdminRole (u' "a0000000-0000-0000-0000-000000000041") differentTenant.uuid differentWorkspaceUuid

differentWorkspaceUserRole :: Role
differentWorkspaceUserRole = workspaceUserRole (u' "a0000000-0000-0000-0000-000000000042") differentTenant.uuid differentWorkspaceUuid

differentAdminRole :: Role
differentAdminRole =
  adminRole
    { uuid = u' "a0000000-0000-0000-0000-000000000011"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }

differentDataStewardRole :: Role
differentDataStewardRole =
  dataStewardRole
    { uuid = u' "a0000000-0000-0000-0000-000000000012"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }

differentResearcherRole :: Role
differentResearcherRole =
  researcherRole
    { uuid = u' "a0000000-0000-0000-0000-000000000013"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }
