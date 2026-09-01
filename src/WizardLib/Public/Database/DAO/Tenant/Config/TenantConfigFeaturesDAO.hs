module WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigFeatures ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

entityName = "config_features"

findTenantConfigFeatures :: AppContextC s sc m => m TenantConfigFeatures
findTenantConfigFeatures = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigFeaturesByUuid tenantUuid

findTenantConfigFeaturesByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigFeatures
findTenantConfigFeaturesByUuid uuid = do
  table <- tableName entityName
  createFindEntityByFn table [("tenant_uuid", U.toString uuid)]

insertTenantConfigFeatures :: AppContextC s sc m => TenantConfigFeatures -> m Int64
insertTenantConfigFeatures config = do
  table <- tableName entityName
  createInsertFn table config

updateTenantConfigFeatures :: AppContextC s sc m => TenantConfigFeatures -> m Int64
updateTenantConfigFeatures config = do
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET tenant_uuid = ?, ai_assistant_enabled = ?, tours_enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
            [table]
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigFeatures :: AppContextC s sc m => m Int64
deleteTenantConfigFeatures = do
  table <- tableName entityName
  createDeleteEntitiesFn table
