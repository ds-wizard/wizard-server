module Shared.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.Config.TenantConfigFeatures ()
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Util.String

entityName = "config_features"

findTenantConfigFeatures :: RequestContextC s sc m => m TenantConfigFeatures
findTenantConfigFeatures = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigFeaturesByUuid tenantUuid

findTenantConfigFeaturesByUuid :: RequestContextC s sc m => U.UUID -> m TenantConfigFeatures
findTenantConfigFeaturesByUuid uuid = do
  createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigFeatures :: RequestContextC s sc m => TenantConfigFeatures -> m Int64
insertTenantConfigFeatures config = do
  createInsertFn entityName config

updateTenantConfigFeatures :: RequestContextC s sc m => TenantConfigFeatures -> m Int64
updateTenantConfigFeatures config = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET tenant_uuid = ?, ai_assistant_enabled = ?, tours_enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
            [entityName]
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigFeatures :: RequestContextC s sc m => m Int64
deleteTenantConfigFeatures = do
  createDeleteEntitiesFn entityName
