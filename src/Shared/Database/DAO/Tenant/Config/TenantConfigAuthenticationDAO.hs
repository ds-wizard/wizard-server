module Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigAuthentication ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

findTenantConfigAuthentication :: WizardRequestContextC s m => m TenantConfigAuthentication
findTenantConfigAuthentication = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigAuthenticationByUuid tenantUuid

findTenantConfigAuthenticationByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigAuthentication
findTenantConfigAuthenticationByUuid tenantUuid =
  createFindEntityByFn "config_authentication" [("tenant_uuid", U.toString tenantUuid)]

insertTenantConfigAuthentication :: WizardRequestContextC s m => TenantConfigAuthentication -> m Int64
insertTenantConfigAuthentication = createInsertFn "config_authentication"

updateTenantConfigAuthentication :: WizardRequestContextC s m => TenantConfigAuthentication -> m Int64
updateTenantConfigAuthentication config = do
  let sql =
        fromString
          "UPDATE config_authentication SET tenant_uuid = ?, default_role_uuid = ?, internal_registration_enabled = ?, internal_two_factor_auth_enabled = ?, internal_two_factor_auth_code_length = ?, internal_two_factor_auth_code_expiration = ?, created_at = ?, updated_at = ?, internal_non_admin_login_enabled = ?, internal_session_expiration = ?, internal_user_email_link_expiration = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigAuthentications :: WizardRequestContextC s m => m Int64
deleteTenantConfigAuthentications = createDeleteEntitiesFn "config_authentication"
