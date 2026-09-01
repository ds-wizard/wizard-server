module WizardServer.Database.DAO.User.UserGroupMembershipDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Common ()
import Shared.Model.Context.WizardRequestContext
import WizardServer.Database.Mapping.User.UserGroupSuggestion ()

entityName = "user_group_membership"

findUserGroupUuidsForUserUuidAndTenantUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m [U.UUID]
findUserGroupUuidsForUserUuidAndTenantUuid userUuid tenantUuid =
  createFindEntitiesWithFieldsByFn "user_group_uuid" entityName [("user_uuid", U.toString userUuid), ("tenant_uuid", U.toString tenantUuid)]
