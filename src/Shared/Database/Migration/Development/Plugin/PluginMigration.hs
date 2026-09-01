module Shared.Database.Migration.Development.Plugin.PluginMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Plugin/Plugin) started"
  deletePlugins
  insertPlugin plugin1
  insertPlugin differentPlugin1
  logInfo _CMP_MIGRATION "(Plugin/Plugin) ended"
