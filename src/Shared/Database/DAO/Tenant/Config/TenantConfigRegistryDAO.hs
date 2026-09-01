module Shared.Database.DAO.Tenant.Config.TenantConfigRegistryDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigRegistry ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

entityName = "config_registry"

findTenantConfigRegistry :: WizardRequestContextC s m => m TenantConfigRegistry
findTenantConfigRegistry = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigRegistryByUuid tenantUuid

findTenantConfigRegistryByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigRegistry
findTenantConfigRegistryByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigRegistry :: WizardRequestContextC s m => TenantConfigRegistry -> m Int64
insertTenantConfigRegistry = createInsertFn entityName

updateTenantConfigRegistry :: WizardRequestContextC s m => TenantConfigRegistry -> m Int64
updateTenantConfigRegistry config = do
  let sql = fromString "UPDATE config_registry SET tenant_uuid = ?, enabled = ?, token = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigRegistries :: WizardRequestContextC s m => m Int64
deleteTenantConfigRegistries = createDeleteEntitiesFn entityName
