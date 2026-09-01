module Shared.Service.Tenant.TenantUtil where

import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Tenant
import Shared.Service.Tenant.TenantMapper

enhanceTenant :: WizardRequestContextC s m => Tenant -> m TenantDTO
enhanceTenant tenant = do
  tcLookAndFeel <- findTenantConfigLookAndFeelByUuid tenant.uuid
  return $ toDTO tenant tcLookAndFeel.logoUrl tcLookAndFeel.primaryColor
