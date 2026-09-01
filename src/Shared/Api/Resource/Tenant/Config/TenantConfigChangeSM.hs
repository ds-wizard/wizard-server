module Shared.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Shared.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import Shared.Api.Resource.Tenant.Config.TenantConfigSM ()
import Shared.Database.Migration.Development.Tenant.Data.TenantConfigs
import Shared.Util.Swagger

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenAnnouncementChangeDto

instance ToSchema TenantConfigLookAndFeelChangeDTO where
  declareNamedSchema = toSwagger defaultLookAndFeelChangeDto

instance ToSchema TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLinkChangeDto

instance ToSchema TenantConfigFeaturesChangeFullDTO where
  declareNamedSchema = toSwagger defaultFeaturesChangeFullDto
