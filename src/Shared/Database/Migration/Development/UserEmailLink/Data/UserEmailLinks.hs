module Shared.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Util.Uuid

registrationUserEmailLink =
  UserEmailLink
    { uuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = userAlbert.uuid
    , aType = RegistrationUserEmailLinkType
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordUserEmailLink =
  UserEmailLink
    { uuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = userAlbert.uuid
    , aType = ForgottenPasswordUserEmailLinkType
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordUserEmailLinkDto =
  UserEmailLinkDTO {aType = forgottenPasswordUserEmailLink.aType, email = userAlbert.email}

differentUserEmailLink =
  UserEmailLink
    { uuid = u' "61feb6c8-3be6-4095-b2e8-7e63dcfd1f31"
    , identity = userCharles.uuid
    , aType = RegistrationUserEmailLinkType
    , hash = "b2da34b1-35b2-408b-8127-b0ab3b8b04d9"
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
