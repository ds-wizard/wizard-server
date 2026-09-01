module Shared.Database.Migration.Development.User.Data.NikolaTesla where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Uuid

userNikola :: User
userNikola =
  User
    { uuid = u' "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , firstName = "Nikola"
    , lastName = "Tesla"
    , email = "nikola.tesla@example.com"
    , affiliation = Nothing
    , role = toRoleSimple dataStewardRole
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , emailPending = Nothing
    }
