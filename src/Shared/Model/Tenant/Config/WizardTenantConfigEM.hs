module Shared.Model.Tenant.Config.WizardTenantConfigEM where

import Shared.Model.Common.SensitiveData
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.TenantConfigEM ()
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Util.Crypto (encryptAES256WithB64)

instance SensitiveData TenantConfigOrganization

instance SensitiveData TenantConfigAuthentication

instance SensitiveData TenantConfigAuthenticationInternal

instance SensitiveData TenantConfigPrivacyAndSupport

instance SensitiveData TenantConfigDashboardAndLoginScreen

instance SensitiveData TenantConfigDashboardAndLoginScreenDashboardType

instance SensitiveData TenantConfigLookAndFeel

instance SensitiveData TenantConfigLookAndFeelCustomMenuLink

instance SensitiveData TenantConfigRegistry where
  process key entity = entity {token = encryptAES256WithB64 key entity.token}

instance SensitiveData TenantConfigProject
instance SensitiveData TenantConfigFeatures
