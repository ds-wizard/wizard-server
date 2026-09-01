module Shared.Database.DAO.Tenant.Module.TenantModuleDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.Module.TenantModule ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Module.TenantModule

entityName = "tenant_module"

findTenantModules :: RequestContextC s sc m => m [TenantModule]
findTenantModules = do
  tenantUuid <- asks (.tenantUuid')
  findTenantModulesByTenantUuid tenantUuid

findTenantModulesByTenantUuid :: RequestContextC s sc m => U.UUID -> m [TenantModule]
findTenantModulesByTenantUuid tenantUuid = do
  createFindEntitiesBySortedFn entityName [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]

insertTenantModule :: RequestContextC s sc m => TenantModule -> m Int64
insertTenantModule tenantModule = do
  createInsertFn entityName tenantModule

deleteTenantModules :: RequestContextC s sc m => m Int64
deleteTenantModules = do
  createDeleteEntitiesFn entityName

deleteTenantModulesByTenantUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteTenantModulesByTenantUuid tenantUuid = do
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid]
