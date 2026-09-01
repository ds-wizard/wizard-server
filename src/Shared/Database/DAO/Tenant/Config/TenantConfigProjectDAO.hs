module Shared.Database.DAO.Tenant.Config.TenantConfigProjectDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigProject ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

entityName = "config_project"

findTenantConfigProject :: WizardRequestContextC s m => m TenantConfigProject
findTenantConfigProject = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigProjectByUuid tenantUuid

findTenantConfigProjectByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigProject
findTenantConfigProjectByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigProject :: WizardRequestContextC s m => TenantConfigProject -> m Int64
insertTenantConfigProject = createInsertFn entityName

updateTenantConfigProject :: WizardRequestContextC s m => TenantConfigProject -> m Int64
updateTenantConfigProject config = do
  let sql =
        fromString
          "UPDATE config_project \
          \SET tenant_uuid = ?, \
          \    visibility_enabled = ?, \
          \    visibility_default_value = ?, \
          \    sharing_enabled = ?, \
          \    sharing_default_value = ?, \
          \    sharing_anonymous_enabled = ?, \
          \    creation = ?, \
          \    project_tagging_enabled = ?, \
          \    project_tagging_tags = ?, \
          \    summary_report = ?, \
          \    created_at = ?, \
          \    updated_at = ? \
          \WHERE tenant_uuid = ?;"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigProjects :: WizardRequestContextC s m => m Int64
deleteTenantConfigProjects = createDeleteEntitiesFn entityName
