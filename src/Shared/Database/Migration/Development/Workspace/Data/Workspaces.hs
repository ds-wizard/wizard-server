module Shared.Database.Migration.Development.Workspace.Data.Workspaces where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import Shared.Util.Uuid

defaultWorkspace :: Workspace
defaultWorkspace =
  Workspace
    { uuid = defaultWorkspaceUuid
    , tenantUuid = defaultTenantUuid
    , name = "Default Tenant"
    , description = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
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
    { uuid = u' "3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9"
    , tenantUuid = defaultTenantUuid
    , name = "Second Workspace"
    , description = Just "The second workspace of the default tenant"
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
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

toMembership :: Workspace -> User -> WorkspaceMembership
toMembership workspace user =
  WorkspaceMembership
    { workspaceUuid = workspace.uuid
    , userUuid = user.uuid
    , tenantUuid = workspace.tenantUuid
    , createdAt = workspace.createdAt
    }

defaultWorkspaceMembership :: User -> WorkspaceMembership
defaultWorkspaceMembership = toMembership defaultWorkspace

secondWorkspaceMembership :: User -> WorkspaceMembership
secondWorkspaceMembership = toMembership secondWorkspace

differentWorkspaceMembership :: User -> WorkspaceMembership
differentWorkspaceMembership = toMembership differentWorkspace
