module Shared.Service.Tenant.TenantUtil where

import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Service.Settings.SettingsService
import Shared.Service.Tenant.TenantMapper

enhanceTenant :: WizardRequestContextC s m => Tenant -> m TenantDTO
enhanceTenant tenant = do
  tcLookAndFeel <- getSettingsByTenantUuid findSettingsLookAndFeel tenant.uuid
  return $ toDTO tenant tcLookAndFeel.logoUrl tcLookAndFeel.primaryColor
