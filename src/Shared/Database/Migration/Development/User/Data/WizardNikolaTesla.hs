module Shared.Database.Migration.Development.User.Data.WizardNikolaTesla (
  module Shared.Database.Migration.Development.User.Data.WizardNikolaTesla,
  module Shared.Database.Migration.Development.User.Data.NikolaTesla,
) where

import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.NikolaTesla
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSuggestion
import Shared.Model.User.UserTour
import Shared.Service.User.WizardUserMapper
import Shared.Util.Date

userNikolaBioGroupMembership :: UserGroupMembership
userNikolaBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userNikola.uuid
    , mType = OwnerUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userNikolaTour1 :: UserTour
userNikolaTour1 =
  UserTour
    { userUuid = userNikola.uuid
    , tourId = "TOUR_1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

userNikolaSuggestionDto :: UserSuggestion
userNikolaSuggestionDto = toSuggestion . toSimple $ userNikola
