module Shared.Api.Resource.Workspace.WorkspaceMemberSM where

import Data.Swagger

import Shared.Api.Resource.User.RoleSimpleSM ()
import Shared.Api.Resource.Workspace.WorkspaceMemberJM ()
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Workspace.WorkspaceMember
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.Workspace.WorkspaceMapper
import Shared.Util.Swagger

instance ToSchema WorkspaceMember where
  declareNamedSchema = toSwagger (toMember (defaultWorkspaceMembership userAlbert) (toRoleSimple defaultWorkspaceUserRole) userAlbert)
