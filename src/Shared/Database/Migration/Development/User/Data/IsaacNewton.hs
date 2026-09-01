module Shared.Database.Migration.Development.User.Data.IsaacNewton where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Uuid

userIsaac :: User
userIsaac =
  User
    { uuid = u' "e1c58e52-0824-4526-8ebe-ec38eec67030"
    , firstName = "Isaac"
    , lastName = "Newton"
    , email = "isaac.newton@example.com"
    , affiliation = Nothing
    , role = toRoleSimple researcherRole
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , emailPending = Nothing
    }
