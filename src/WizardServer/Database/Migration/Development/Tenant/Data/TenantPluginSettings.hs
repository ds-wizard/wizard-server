module WizardServer.Database.Migration.Development.Tenant.Data.TenantPluginSettings where

import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.Plugin.Plugin
import Shared.Model.Tenant.Tenant
import Shared.Util.Date
import WizardServer.Model.Tenant.PluginSettings.TenantPluginSettings

defaultTenantPluginSettings :: TenantPluginSettings
defaultTenantPluginSettings =
  TenantPluginSettings
    { tenantUuid = defaultTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values1
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

defaultTenantPluginSettingsEdited :: TenantPluginSettings
defaultTenantPluginSettingsEdited =
  defaultTenantPluginSettings
    { tenantUuid = defaultTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values1Edited
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

differentTenantPluginSettings :: TenantPluginSettings
differentTenantPluginSettings =
  TenantPluginSettings
    { tenantUuid = differentTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values2
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
