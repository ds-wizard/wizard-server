module WizardServer.Database.DAO.User.UserPluginSettingsDAO where

import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserPluginSettings
import WizardServer.Database.Mapping.User.UserPluginSettings ()

entityName = "user_plugin_settings"

findUserPluginSettingValuesByUserUuidAndTenantUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (M.Map U.UUID A.Value)
findUserPluginSettingValuesByUserUuidAndTenantUuid userUuid tenantUuid = do
  let sql =
        fromString
          "SELECT plugin_uuid, values \
          \FROM user_plugin_settings \
          \WHERE tenant_uuid = ? \
          \  AND user_uuid = ?;"
  let params = [toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

findUserPluginSettingsByUserUuidAndPluginUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m UserPluginSettings
findUserPluginSettingsByUserUuidAndPluginUuid userUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("plugin_uuid", U.toString pluginUuid)]

findUserPluginSettingsByUserUuidAndPluginUuid' :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (Maybe UserPluginSettings)
findUserPluginSettingsByUserUuidAndPluginUuid' userUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("plugin_uuid", U.toString pluginUuid)]

insertUserPluginSettings :: WizardRequestContextC s m => UserPluginSettings -> m Int64
insertUserPluginSettings = createInsertFn entityName

updateUserPluginSettings :: WizardRequestContextC s m => UserPluginSettings -> m Int64
updateUserPluginSettings pluginSettings = do
  let sql =
        fromString
          "UPDATE user_plugin_settings SET user_uuid = ?, plugin_uuid = ?,  values = ?,  tenant_uuid = ?,  created_at = ?, updated_at = ? WHERE user_uuid = ? AND plugin_uuid = ?"
  let params = toRow pluginSettings ++ [toField pluginSettings.userUuid, toField pluginSettings.pluginUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
