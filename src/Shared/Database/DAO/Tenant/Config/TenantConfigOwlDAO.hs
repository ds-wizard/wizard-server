module Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigOwl ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

entityName = "config_owl"

findTenantConfigOwl :: WizardRequestContextC s m => m TenantConfigOwl
findTenantConfigOwl = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigOwlByUuid tenantUuid

findTenantConfigOwlByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigOwl
findTenantConfigOwlByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigOwl :: WizardRequestContextC s m => TenantConfigOwl -> m Int64
insertTenantConfigOwl = createInsertFn entityName

updateTenantConfigOwl :: WizardRequestContextC s m => TenantConfigOwl -> m Int64
updateTenantConfigOwl config = do
  let sql = fromString "UPDATE config_owl SET tenant_uuid = ?, enabled = ?, name = ?, organization_id = ?, km_id = ?, version = ?, previous_package_uuid = ?, root_element = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigOwls :: WizardRequestContextC s m => m Int64
deleteTenantConfigOwls = createDeleteEntitiesFn entityName
