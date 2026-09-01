module Shared.Database.DAO.Plugin.PluginDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Plugin.Plugin ()
import Shared.Database.Mapping.Plugin.PluginList ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.PluginList

entityName = "plugin"

findPlugins :: WizardRequestContextC s m => U.UUID -> m [PluginList]
findPlugins tenantUuid = do
  createFindEntitiesWithFieldsByFn "uuid, url, enabled" entityName [tenantQueryUuid tenantUuid]

insertPlugin :: WizardRequestContextC s m => Plugin -> m Int64
insertPlugin = createInsertFn entityName

updatePluginEnabled :: WizardRequestContextC s m => U.UUID -> Bool -> m Int64
updatePluginEnabled uuid enabled = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE plugin SET enabled = ?, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField enabled, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePluginUrlForAllTenants :: WizardRequestContextC s m => U.UUID -> String -> m Int64
updatePluginUrlForAllTenants uuid url = do
  let sql = fromString "UPDATE plugin SET url = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField url, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePluginUrlForTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> String -> m Int64
updatePluginUrlForTenant tenantUuid uuid url = do
  let sql = fromString "UPDATE plugin SET url = ?, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField url, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePlugins :: WizardRequestContextC s m => m Int64
deletePlugins = createDeleteEntitiesFn entityName

deletePluginForAllTenants :: WizardRequestContextC s m => U.UUID -> m Int64
deletePluginForAllTenants pluginUuid = createDeleteEntityByFn entityName [("uuid", U.toString pluginUuid)]

deletePluginForTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> m Int64
deletePluginForTenant tenantUuid pluginUuid = createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString pluginUuid)]
