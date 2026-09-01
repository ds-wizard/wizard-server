module Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.Config.TenantConfigOrganization ()
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

entityName = "config_organization"

findTenantConfigOrganization :: RequestContextC s sc m => m TenantConfigOrganization
findTenantConfigOrganization = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigOrganizationByUuid tenantUuid

findTenantConfigOrganizationByUuid :: RequestContextC s sc m => U.UUID -> m TenantConfigOrganization
findTenantConfigOrganizationByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigOrganization :: RequestContextC s sc m => TenantConfigOrganization -> m Int64
insertTenantConfigOrganization = createInsertFn entityName

updateTenantConfigOrganization :: RequestContextC s sc m => TenantConfigOrganization -> m Int64
updateTenantConfigOrganization config = do
  let sql =
        fromString
          "UPDATE config_organization SET tenant_uuid = ?, name = ?, description = ?, organization_id = ?, affiliations = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigOrganizations :: RequestContextC s sc m => m Int64
deleteTenantConfigOrganizations = createDeleteEntitiesFn entityName
