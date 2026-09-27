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
    { uuid = u' "378bdd91-96e4-44ad-9b39-25cd3cdde18d"
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
    { uuid = u' "65f88e18-8a24-4f70-92e7-8f20f7559600"
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
    { uuid = u' "4e79c242-4100-4447-becd-45eca47fa60b"
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
    { uuid = u' "1df6b458-304f-440b-b9e6-2f87e75f2eee"
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
    { uuid = u' "8261f971-eaa9-44bf-ba35-fb5cb0dc92eb"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }

differentDataStewardRole :: Role
differentDataStewardRole =
  dataStewardRole
    { uuid = u' "3f09af4e-07b1-4d61-b79f-e2689a6903f4"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }

differentResearcherRole :: Role
differentResearcherRole =
  researcherRole
    { uuid = u' "59f1d8c9-9ba5-4063-8ee8-e75d752a2c3c"
    , tenantUuid = differentTenant.uuid
    , workspaceUuid = Nothing
    }
