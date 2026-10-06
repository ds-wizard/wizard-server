module Shared.Database.Migration.Development.User.Data.WizardAlbertEinstein (
  module Shared.Database.Migration.Development.User.Data.WizardAlbertEinstein,
  module Shared.Database.Migration.Development.User.Data.AlbertEinstein,
) where

import qualified Data.Map.Strict as M

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Api.Resource.User.UserStateDTO
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.AlbertEinstein
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.Common.SensitiveData
import Shared.Model.Locale.Locale
import Shared.Model.Plugin.Plugin
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserPluginSettings
import Shared.Model.User.UserProfile
import Shared.Model.User.UserSubmissionProp
import Shared.Model.User.UserSubmissionPropEM ()
import Shared.Model.User.UserSubmissionPropList
import Shared.Model.User.UserSuggestion
import Shared.Model.User.UserTour
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.User.WizardUserMapper
import Shared.Util.Date

userAlbertEdited :: User
userAlbertEdited =
  userAlbert
    { firstName = "EDITED: Isaac"
    , lastName = "EDITED: Einstein"
    , email = "albert.einstein@example-edited.com"
    , affiliation = Just "EDITED: My University"
    }

userAlbertEditedAfterPut :: User
userAlbertEditedAfterPut =
  userAlbertEdited
    { email = userAlbert.email
    , emailVerifiedAt = Nothing
    , emailPending = Just userAlbertEdited.email
    }

userAlbertWithNewsId :: User
userAlbertWithNewsId =
  userAlbert
    { lastSeenNewsId = Just "my-news-id"
    }

userAlbertDto :: UserDTO
userAlbertDto = toDTO userAlbert

userAlbertProfile :: UserProfile
userAlbertProfile = toUserProfile (toDTO userAlbert) [bioGroup.uuid] plugin1Dict (M.singleton defaultWorkspaceUuid (toRoleSimple defaultWorkspaceUserRole))

userAlbertEditedChange :: UserProfileChangeDTO
userAlbertEditedChange =
  UserProfileChangeDTO
    { firstName = userAlbertEdited.firstName
    , lastName = userAlbertEdited.lastName
    , email = userAlbertEdited.email
    , affiliation = userAlbertEdited.affiliation
    }

userPassword :: UserPasswordDTO
userPassword = UserPasswordDTO {password = "newPassword"}

userState :: UserStateDTO
userState = UserStateDTO {active = True}

userAlbertOnlineInfo :: OnlineUserInfo
userAlbertOnlineInfo = toLoggedOnlineUserInfo (toDTO userAlbert) 10 [bioGroup.uuid]

userAlbertSuggestion :: UserSuggestion
userAlbertSuggestion = toSuggestion . toSimple $ userAlbert

userAlbertBioGroupMembership :: UserGroupMembership
userAlbertBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userAlbert.uuid
    , mType = OwnerUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userAlbertTour1 :: UserTour
userAlbertTour1 =
  UserTour
    { userUuid = userAlbert.uuid
    , tourId = "TOUR_1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

userAlbertTour2 :: UserTour
userAlbertTour2 =
  UserTour
    { userUuid = userAlbert.uuid
    , tourId = "TOUR_2"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

userAlbertPluginSettings :: UserPluginSettings
userAlbertPluginSettings =
  UserPluginSettings
    { userUuid = userAlbert.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values1
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userAlbertPluginSettingsEdited :: UserPluginSettings
userAlbertPluginSettingsEdited =
  userAlbertPluginSettings
    { values = plugin1Values1Edited
    }

-- --------------------------------------
-- SUBMISSION
-- --------------------------------------
userAlbertSubmissionProps :: [UserSubmissionProp]
userAlbertSubmissionProps = [process defaultSecret userAlbertApiToken]

userAlbertApiToken :: UserSubmissionProp
userAlbertApiToken =
  UserSubmissionProp
    { userUuid = userAlbert.uuid
    , serviceId = settingsSubmissionService.sId
    , values = M.fromList [(settingsSubmissionServiceSecretProp, ""), (settingsSubmissionServiceApiTokenProp, "Some Token")]
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userAlbertApiTokenList :: UserSubmissionPropList
userAlbertApiTokenList =
  UserSubmissionPropList
    { sId = settingsSubmissionService.sId
    , name = settingsSubmissionService.name
    , values = M.fromList [(settingsSubmissionServiceSecretProp, ""), (settingsSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertSubmissionPropsEdited :: [UserSubmissionProp]
userAlbertSubmissionPropsEdited = [process defaultSecret userAlbertApiTokenEdited]

userAlbertApiTokenEdited :: UserSubmissionProp
userAlbertApiTokenEdited =
  userAlbertApiToken
    { values = M.fromList [(settingsSubmissionServiceSecretProp, ""), (settingsSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }

userAlbertApiTokenEditedDto :: UserSubmissionPropList
userAlbertApiTokenEditedDto =
  UserSubmissionPropList
    { sId = settingsSubmissionService.sId
    , name = settingsSubmissionService.name
    , values = M.fromList [(settingsSubmissionServiceSecretProp, ""), (settingsSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }

userAlbertEditedLocale :: User
userAlbertEditedLocale =
  userAlbert
    { locale = Just localeNl.uuid
    }
