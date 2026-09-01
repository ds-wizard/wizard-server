module Shared.Database.DAO.Tenant.TenantLimitBundleDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.TenantLimitBundle ()
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Limit.TenantLimitBundle

entityName = "tenant_limit_bundle"

findLimitBundleByUuid :: RequestContextC s sc m => U.UUID -> m TenantLimitBundle
findLimitBundleByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findLimitBundleForCurrentTenant :: RequestContextC s sc m => m TenantLimitBundle
findLimitBundleForCurrentTenant = do
  tenantUuid <- asks (.tenantUuid')
  findLimitBundleByUuid tenantUuid

insertLimitBundle :: RequestContextC s sc m => TenantLimitBundle -> m Int64
insertLimitBundle = createInsertFn entityName

updateLimitBundleByUuid :: RequestContextC s sc m => TenantLimitBundle -> m TenantLimitBundle
updateLimitBundleByUuid limitBundle = do
  now <- liftIO getCurrentTime
  let updatedTenantLimitBundle = limitBundle {updatedAt = now}
  let sql =
        fromString
          "UPDATE tenant_limit_bundle SET uuid = ?, users = ?, active_users = ?, knowledge_models = ?, knowledge_model_editors = ?, document_templates = ?, projects = ?, documents =?, storage = ?, created_at = ?, updated_at = ?, document_template_drafts = ?, locales = ? WHERE uuid = ?"
  let params = toRow updatedTenantLimitBundle ++ [toField updatedTenantLimitBundle.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedTenantLimitBundle

deleteLimitBundles :: RequestContextC s sc m => m Int64
deleteLimitBundles = createDeleteEntitiesFn entityName
