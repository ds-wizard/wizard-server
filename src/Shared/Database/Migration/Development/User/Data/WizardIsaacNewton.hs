module Shared.Database.Migration.Development.User.Data.WizardIsaacNewton (
  module Shared.Database.Migration.Development.User.Data.WizardIsaacNewton,
  module Shared.Database.Migration.Development.User.Data.IsaacNewton,
) where

import Shared.Api.Resource.User.UserChangeDTO
import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.IsaacNewton
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardAlbertEinstein
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Date

userIsaacEdited :: User
userIsaacEdited =
  userAlbert
    { firstName = "EDITED: Isaac"
    , lastName = "EDITED: Newton"
    , email = "albert.einstein@example.com"
    , affiliation = Just "EDITED: My University"
    , role = toRoleSimple dataStewardRole
    , active = True
    }

userIsaacEditedChange :: UserChangeDTO
userIsaacEditedChange =
  UserChangeDTO
    { firstName = userIsaacEdited.firstName
    , lastName = userIsaacEdited.lastName
    , email = userIsaacEdited.email
    , affiliation = userIsaacEdited.affiliation
    , roleUuid = userIsaacEdited.role.uuid
    , active = userIsaacEdited.active
    }

userIsaacProfileChange :: UserProfileChangeDTO
userIsaacProfileChange =
  UserProfileChangeDTO
    { firstName = userAlbertEdited.firstName
    , lastName = userAlbertEdited.lastName
    , email = userAlbertEdited.email
    , affiliation = userAlbertEdited.affiliation
    }

userIsaacBioGroupMembership :: UserGroupMembership
userIsaacBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userIsaacPlantGroupMembership :: UserGroupMembership
userIsaacPlantGroupMembership =
  UserGroupMembership
    { userGroupUuid = plantGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userIsaacAnimalGroupMembership :: UserGroupMembership
userIsaacAnimalGroupMembership =
  UserGroupMembership
    { userGroupUuid = animalGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
