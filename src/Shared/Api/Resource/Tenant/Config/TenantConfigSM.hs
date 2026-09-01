module Shared.Api.Resource.Tenant.Config.TenantConfigSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.Config.TenantConfigJM ()
import Shared.Database.Migration.Development.Tenant.Data.TenantConfigs
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Util.Swagger

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncement where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenAnnouncement

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance ToSchema TenantConfigLookAndFeel where
  declareNamedSchema = toSwagger defaultLookAndFeel

instance ToSchema TenantConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLink

instance ToSchema TenantConfigFeatures where
  declareNamedSchema = toSwagger defaultFeatures
