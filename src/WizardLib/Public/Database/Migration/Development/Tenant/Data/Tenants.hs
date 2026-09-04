module WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.Tenant
import WizardLib.Public.Model.Tenant.Tenant
import WizardLib.Public.Model.Tenant.TenantSuggestion

defaultTenant :: Tenant
defaultTenant =
  Tenant
    { uuid = defaultTenantUuid
    , tenantId = "default"
    , name = "Default Tenant"
    , serverDomain = "localhost:3000"
    , serverUrl = "http://localhost:3000"
    , clientUrl = "http://localhost:8080"
    , enabled = True
    , state = ReadyForUseTenantState
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentTenant :: Tenant
differentTenant =
  Tenant
    { uuid = differentTenantUuid
    , tenantId = "different"
    , name = "Different Tenant"
    , serverDomain = "different-server.example.com"
    , serverUrl = "https://different-server.example.com"
    , clientUrl = "https://different-client.example.com"
    , enabled = True
    , state = ReadyForUseTenantState
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentTenantEdited :: Tenant
differentTenantEdited =
  differentTenant
    { tenantId = "different-edited"
    , name = "EDtIED:Different Tenant"
    , serverDomain = "different-edited."
    , serverUrl = "https://different-edited."
    , clientUrl = "https://different-edited."
    }

tenantSuggestion :: TenantSuggestion
tenantSuggestion =
  TenantSuggestion
    { uuid = defaultTenantUuid
    , name = "Default Tenant"
    , logoUrl = Nothing
    , primaryColor = Nothing
    , clientUrl = "http://localhost:8080/wizard"
    }
