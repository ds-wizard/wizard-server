module Shared.Api.Resource.Tenant.Config.TenantConfigJM where

import Data.Aeson

import Shared.Model.Tenant.Config.TenantConfig
import Shared.Util.Aeson

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncement where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncement where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance FromJSON TenantConfigLookAndFeel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeel where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelCustomMenuLink where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelCustomMenuLink where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigFeatures where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigFeatures where
  toJSON = genericToJSON jsonOptions
