module Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles where

import Shared.Constant.Tenant
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.Tenant.Limit.TenantLimitBundle
import Shared.Model.Tenant.Tenant
import Shared.Util.Date

defaultTenantLimitBundle :: TenantLimitBundle
defaultTenantLimitBundle =
  TenantLimitBundle
    { uuid = defaultTenantUuid
    , users = -1000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , knowledgeModelEditors = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , projects = -1000
    , documents = -1000
    , locales = -1000
    , storage = (-1000) * 5 * 1000 * 1000
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

defaultTenantLimitBundleEdited :: TenantLimitBundle
defaultTenantLimitBundleEdited =
  defaultTenantLimitBundle
    { users = -2000
    }

differentTenantLimitBundle :: TenantLimitBundle
differentTenantLimitBundle =
  TenantLimitBundle
    { uuid = differentTenant.uuid
    , users = -1000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , knowledgeModelEditors = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , projects = -1000
    , documents = -1000
    , locales = -1000
    , storage = (-1000) * 5 * 1000 * 1000
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
