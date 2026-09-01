module Shared.Database.Migration.Development.User.Data.SystemUser where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.User
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)

userSystem :: User
userSystem =
  User
    { uuid = systemUserUuid
    , firstName = "System"
    , lastName = "User"
    , email = "system@example.com"
    , affiliation = Nothing
    , role = (toRoleSimple adminRole) {permissions = allRolePermissions ++ [_DEV_USE_ROLE_PERMISSION, _TENANTS_MANAGE_ROLE_PERMISSION]}
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = True
    , lastSeenNewsId = Nothing
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , emailPending = Nothing
    }
