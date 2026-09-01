module Shared.Database.Migration.Development.User.Data.AlbertEinstein where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Uuid

userAlbert :: User
userAlbert =
  User
    { uuid = u' "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , firstName = "Albert"
    , lastName = "Einstein"
    , email = "albert.einstein@example.com"
    , affiliation = Just "My University"
    , role = (toRoleSimple adminRole) {permissions = allRolePermissions ++ [_DEV_USE_ROLE_PERMISSION, _TENANTS_MANAGE_ROLE_PERMISSION]}
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , emailPending = Nothing
    }
