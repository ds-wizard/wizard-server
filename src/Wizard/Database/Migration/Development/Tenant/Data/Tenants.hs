module Wizard.Database.Migration.Development.Tenant.Data.Tenants (
  module WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants,
  defaultTenantModules,
  defaultTenantModule,
  tenantCreateDto,
) where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.Tenant
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants
import WizardLib.Public.Model.Tenant.Module.TenantModule
import WizardLib.Public.Model.User.RolePermission

defaultTenantModules :: [TenantModule]
defaultTenantModules =
  [ defaultTenantModule {position = 0, moduleKey = "wizard", title = "Wizard", url = "http://localhost:8080/wizard"}
  , defaultTenantModule {position = 1, moduleKey = "admin", title = "Administration", url = "http://localhost:8080/admin"}
  , defaultTenantModule
      { position = 2
      , moduleKey = "integrationHub"
      , title = "Integration Hub"
      , url = "http://localhost:8080/integration-hub"
      , requiredPermission = Just _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
      }
  , defaultTenantModule
      { position = 3
      , moduleKey = "analytics"
      , title = "Analytics"
      , url = "http://localhost:8080/analytics"
      , requiredPermission = Just _SETTINGS_MANAGE_ROLE_PERMISSION
      }
  ]

defaultTenantModule :: TenantModule
defaultTenantModule =
  TenantModule
    { tenantUuid = defaultTenantUuid
    , position = 0
    , moduleKey = "wizard"
    , title = ""
    , description = ""
    , icon = ""
    , url = ""
    , external = False
    , requiredPermission = Nothing
    , enabled = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

tenantCreateDto :: TenantCreateDTO
tenantCreateDto =
  TenantCreateDTO
    { tenantId = "new-tenant-id"
    , tenantName = "New Tenant"
    , firstName = "Max"
    , lastName = "Planck"
    , email = "max.planck@example.com"
    , password = "password"
    }
