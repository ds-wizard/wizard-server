module Shared.Database.Mapping.Workspace.WorkspaceMember where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Model.User.RoleSimple
import Shared.Model.Workspace.WorkspaceMember
import Shared.Util.Gravatar

instance FromRow WorkspaceMember where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    let gravatarHash = createGravatarHash email
    imageUrl <- field
    createdAt <- field
    roleUuid <- field
    roleName <- field
    rolePermissions <- fromPGArray <$> field
    let role = RoleSimple {uuid = roleUuid, name = roleName, permissions = rolePermissions}
    return $ WorkspaceMember {..}
