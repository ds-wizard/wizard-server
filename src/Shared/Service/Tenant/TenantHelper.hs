module Shared.Service.Tenant.TenantHelper where

import Control.Monad.Reader (asks)

import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Tenant
import Shared.Service.Tenant.TenantMapper

getCurrentTenant :: WizardRequestContextC s m => m Tenant
getCurrentTenant = do
  tntUuid <- asks (.tenantUuid')
  findTenantByUuid tntUuid

getClientUrl :: WizardRequestContextC s m => m String
getClientUrl = do
  serverConfig <- asks (.serverConfig')
  if serverConfig.cloud.enabled
    then do
      tenant <- getCurrentTenant
      return $ tenantClientUrl tenant
    else return serverConfig.general.clientUrl
