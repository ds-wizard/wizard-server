module Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.Config.TenantConfigMail ()
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Config.TenantConfig

entityName = "config_mail"

findTenantConfigMail :: RequestContextC s sc m => m TenantConfigMail
findTenantConfigMail = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigMailByUuid tenantUuid

findTenantConfigMailByUuid :: RequestContextC s sc m => U.UUID -> m TenantConfigMail
findTenantConfigMailByUuid uuid = do
  createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigMail :: RequestContextC s sc m => TenantConfigMail -> m Int64
insertTenantConfigMail config = do
  createInsertFn entityName config

deleteTenantConfigMails :: RequestContextC s sc m => m Int64
deleteTenantConfigMails = do
  createDeleteEntitiesFn entityName
