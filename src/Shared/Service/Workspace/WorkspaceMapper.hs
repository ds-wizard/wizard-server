module Shared.Service.Workspace.WorkspaceMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
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
    }

fromChange :: Workspace -> WorkspaceChangeDTO -> UTCTime -> Workspace
fromChange workspace reqDto now =
  workspace
    { name = reqDto.name
    , description = reqDto.description
    , updatedAt = now
    }

toMembership :: U.UUID -> U.UUID -> U.UUID -> UTCTime -> WorkspaceMembership
toMembership workspaceUuid userUuid tenantUuid now =
  WorkspaceMembership
    { workspaceUuid = workspaceUuid
    , userUuid = userUuid
    , tenantUuid = tenantUuid
    , createdAt = now
    }

toMember :: WorkspaceMembership -> User -> WorkspaceMember
toMember membership user =
  WorkspaceMember
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , imageUrl = user.imageUrl
    , createdAt = membership.createdAt
    }
