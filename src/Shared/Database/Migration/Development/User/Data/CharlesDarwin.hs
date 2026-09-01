module Shared.Database.Migration.Development.User.Data.CharlesDarwin where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Uuid

userCharles :: User
userCharles =
  User
    { uuid = u' "1693daf3-fc48-4a93-a1fb-385e6c9fe7ac"
    , firstName = "Charles"
    , lastName = "Darwin"
    , email = "charles.darwin@example.com"
    , affiliation = Nothing
    , role = (toRoleSimple researcherRole) {uuid = differentResearcherRole.uuid}
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = differentTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , emailPending = Nothing
    }
