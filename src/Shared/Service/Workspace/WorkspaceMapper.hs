module Shared.Service.Workspace.WorkspaceMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Model.User.Role
import Shared.Model.User.RoleSimple
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMember
import Shared.Model.Workspace.WorkspaceMembership

fromChangeDTO :: WorkspaceChangeDTO -> U.UUID -> U.UUID -> UTCTime -> Workspace
fromChangeDTO reqDto uuid tenantUuid now =
  Workspace
    { uuid = uuid
    , tenantUuid = tenantUuid
    , name = reqDto.name
    , description = reqDto.description
    , createdAt = now
    , updatedAt = now
    , defaultRoleUuid = Nothing
    , logo = Nothing
    , primaryColor = reqDto.primaryColor
    }

fromChange :: Workspace -> WorkspaceChangeDTO -> UTCTime -> Workspace
fromChange workspace reqDto now =
  workspace
    { name = reqDto.name
    , description = reqDto.description
    , primaryColor = reqDto.primaryColor
    , updatedAt = now
    }

toWorkspaceRole :: U.UUID -> String -> [String] -> Workspace -> Role
toWorkspaceRole uuid name permissions workspace =
  Role
    { uuid = uuid
    , name = name
    , permissions = permissions
    , isAdmin = False
    , tenantUuid = workspace.tenantUuid
    , workspaceUuid = Just workspace.uuid
    , createdAt = workspace.createdAt
    , updatedAt = workspace.createdAt
    }

toMembership :: U.UUID -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> WorkspaceMembership
toMembership workspaceUuid userUuid roleUuid tenantUuid now =
  WorkspaceMembership
    { workspaceUuid = workspaceUuid
    , userUuid = userUuid
    , tenantUuid = tenantUuid
    , createdAt = now
    , roleUuid = roleUuid
    }

toMember :: WorkspaceMembership -> RoleSimple -> User -> WorkspaceMember
toMember membership role user =
  WorkspaceMember
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    , createdAt = membership.createdAt
    , role = role
    }
