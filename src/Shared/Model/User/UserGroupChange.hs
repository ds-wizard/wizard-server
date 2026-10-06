module Shared.Model.User.UserGroupChange where

import Shared.Model.User.UserGroup
import Shared.Model.User.UserWithMembership

data UserGroupChange
  = UserGroupCreated UserGroup
  | UserGroupUpdated UserGroup UserGroup
  | UserGroupDeleted UserGroup
  | UserGroupMembersUpdated UserGroup [UserWithMembership] [UserWithMembership]
