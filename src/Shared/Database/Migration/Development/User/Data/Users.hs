module Shared.Database.Migration.Development.User.Data.Users (
  module Shared.Database.Migration.Development.User.Data.AlbertEinstein,
  module Shared.Database.Migration.Development.User.Data.CharlesDarwin,
  module Shared.Database.Migration.Development.User.Data.GalileoGalilei,
  module Shared.Database.Migration.Development.User.Data.IsaacNewton,
  module Shared.Database.Migration.Development.User.Data.NicolausCopernicus,
  module Shared.Database.Migration.Development.User.Data.NikolaTesla,
  module Shared.Database.Migration.Development.User.Data.SystemUser,
  userAlbertWithMembership,
  userAlbertSuggestion,
  userLocaleEmpty,
  userLocale,
) where

import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Api.Resource.User.UserWithMembershipDTO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Database.Migration.Development.User.Data.AlbertEinstein
import Shared.Database.Migration.Development.User.Data.CharlesDarwin
import Shared.Database.Migration.Development.User.Data.GalileoGalilei
import Shared.Database.Migration.Development.User.Data.IsaacNewton
import Shared.Database.Migration.Development.User.Data.NicolausCopernicus
import Shared.Database.Migration.Development.User.Data.NikolaTesla
import Shared.Database.Migration.Development.User.Data.SystemUser
import Shared.Model.Locale.Locale
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserSuggestion
import Shared.Util.Uuid

userAlbertWithMembership :: UserWithMembershipDTO
userAlbertWithMembership =
  UserWithMembershipDTO
    { uuid = u' "7751d775-1d5e-4a43-9dc8-e43cd76f0884"
    , firstName = "Albert"
    , lastName = "Einstein"
    , gravatarHash = ".."
    , imageUrl = Nothing
    , affiliation = Nothing
    , membershipType = OwnerUserGroupMembershipType
    }

userAlbertSuggestion :: UserSuggestion
userAlbertSuggestion =
  UserSuggestion
    { uuid = u' "7751d775-1d5e-4a43-9dc8-e43cd76f0884"
    , firstName = "Albert"
    , lastName = "Einstein"
    , gravatarHash = ".."
    , imageUrl = Nothing
    , affiliation = Nothing
    }

userLocaleEmpty :: UserLocaleDTO
userLocaleEmpty = UserLocaleDTO {uuid = Nothing}

userLocale :: UserLocaleDTO
userLocale = UserLocaleDTO {uuid = Just localeNl.uuid}
