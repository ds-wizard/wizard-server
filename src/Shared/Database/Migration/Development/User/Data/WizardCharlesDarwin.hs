module Shared.Database.Migration.Development.User.Data.WizardCharlesDarwin (
  module Shared.Database.Migration.Development.User.Data.WizardCharlesDarwin,
  module Shared.Database.Migration.Development.User.Data.CharlesDarwin,
) where

import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.CharlesDarwin
import Shared.Model.Plugin.Plugin
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.User.UserPluginSettings
import Shared.Util.Date

userCharlesPluginSettings :: UserPluginSettings
userCharlesPluginSettings =
  UserPluginSettings
    { userUuid = userCharles.uuid
    , pluginUuid = differentPlugin1.uuid
    , values = plugin1Values2
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
