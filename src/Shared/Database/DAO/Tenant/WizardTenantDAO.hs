module Shared.Database.DAO.Tenant.WizardTenantDAO where

import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Tenant ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Tenant
import Shared.Util.String

entityName = "tenant"

pageLabel = "tenants"

findTenants :: WizardRequestContextC s m => m [Tenant]
findTenants = createFindEntitiesFn entityName

findTenantsPage :: WizardRequestContextC s m => Maybe String -> Maybe [TenantState] -> Maybe Bool -> Pageable -> [Sort] -> m (Page Tenant)
findTenantsPage mQuery mStates mEnabled pageable sort = do
  let statesCondition =
        case mStates of
          Nothing -> ""
          Just [] -> ""
          Just states ->
            let xs = fmap (\s -> f' "state = '%s'" [show s]) states
             in " AND (" ++ L.intercalate " OR " xs ++ ")"
  let enabledCondition =
        case mEnabled of
          Nothing -> ""
          Just True -> " AND enabled = true"
          Just False -> " AND enabled = false"
  let condition = f' "WHERE (name ~* ? OR tenant_id ~* ?) %s %s" [statesCondition, enabledCondition]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition [regexM mQuery, regexM mQuery]

findTenantByUuid :: WizardRequestContextC s m => U.UUID -> m Tenant
findTenantByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findTenantByServerDomain :: WizardRequestContextC s m => String -> m Tenant
findTenantByServerDomain serverDomain = createFindEntityByFn entityName [("server_domain", serverDomain)]

findTenantByServerDomain' :: WizardRequestContextC s m => String -> m (Maybe Tenant)
findTenantByServerDomain' serverDomain = createFindEntityByFn' entityName [("server_domain", serverDomain)]

findTenantByClientUrl :: WizardRequestContextC s m => String -> m Tenant
findTenantByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

insertTenant :: WizardRequestContextC s m => Tenant -> m Int64
insertTenant = createInsertFn entityName

updateTenantByUuid :: WizardRequestContextC s m => Tenant -> m Tenant
updateTenantByUuid tenant = do
  now <- liftIO getCurrentTime
  let updatedTenant = tenant {updatedAt = now}
  let sql =
        fromString
          "UPDATE tenant SET uuid = ?, tenant_id = ?, name = ?, server_domain = ?, client_url = ?, enabled = ?, created_at = ?, updated_at = ?, server_url = ?, state = ? WHERE uuid = ?"
  let params = toRow tenant ++ [toField updatedTenant.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedTenant

deleteTenants :: WizardRequestContextC s m => m Int64
deleteTenants = createDeleteEntitiesFn entityName

deleteTenantByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteTenantByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
