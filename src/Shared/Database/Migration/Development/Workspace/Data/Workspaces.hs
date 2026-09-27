module Shared.Database.Migration.Development.Workspace.Data.Workspaces where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership

defaultWorkspace :: Workspace
defaultWorkspace =
  Workspace
    { uuid = defaultWorkspaceUuid
    , tenantUuid = defaultTenantUuid
    , name = "Default Tenant"
    , description = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , defaultRoleUuid = Just defaultWorkspaceUserRole.uuid
    }

defaultWorkspaceEdited :: Workspace
defaultWorkspaceEdited =
  defaultWorkspace
    { name = "EDITED: Default Tenant"
    , description = Just "EDITED: description"
    }

secondWorkspace :: Workspace
secondWorkspace =
  Workspace
    { uuid = secondWorkspaceUuid
    , tenantUuid = defaultTenantUuid
    , name = "Second Workspace"
    , description = Just "The second workspace of the default tenant"
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , defaultRoleUuid = Just secondWorkspaceUserRole.uuid
    }

differentWorkspace :: Workspace
differentWorkspace =
  Workspace
    { uuid = differentWorkspaceUuid
    , tenantUuid = differentTenantUuid
    , name = "Different Tenant"
    , description = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , defaultRoleUuid = Just differentWorkspaceUserRole.uuid
    }

workspaceCreate :: WorkspaceChangeDTO
workspaceCreate =
  WorkspaceChangeDTO
    { name = "New Workspace"
    , description = Just "A new workspace"
    }

workspaceChange :: WorkspaceChangeDTO
workspaceChange =
  WorkspaceChangeDTO
    { name = "EDITED: Default Tenant"
    , description = Just "EDITED: description"
    }

workspaceMemberChange :: WorkspaceMemberChangeDTO
workspaceMemberChange = WorkspaceMemberChangeDTO {roleUuid = Just defaultWorkspaceAdminRole.uuid}

toMembership :: Workspace -> User -> WorkspaceMembership
toMembership workspace user =
  WorkspaceMembership
    { workspaceUuid = workspace.uuid
    , userUuid = user.uuid
    , tenantUuid = workspace.tenantUuid
    , createdAt = workspace.createdAt
    , roleUuid = fromJust workspace.defaultRoleUuid
    }

toMembershipWithRole :: Workspace -> Role -> User -> WorkspaceMembership
toMembershipWithRole workspace role user = (toMembership workspace user) {roleUuid = role.uuid}

defaultWorkspaceMembership :: User -> WorkspaceMembership
defaultWorkspaceMembership = toMembership defaultWorkspace

secondWorkspaceMembership :: User -> WorkspaceMembership
secondWorkspaceMembership = toMembership secondWorkspace

differentWorkspaceMembership :: User -> WorkspaceMembership
differentWorkspaceMembership = toMembership differentWorkspace
