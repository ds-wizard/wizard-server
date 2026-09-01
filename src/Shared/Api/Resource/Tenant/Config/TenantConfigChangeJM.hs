module Shared.Api.Resource.Tenant.Config.TenantConfigChangeJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Shared.Api.Resource.Tenant.Config.TenantConfigJM ()
import Shared.Util.Aeson

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigFeaturesChangeFullDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigFeaturesChangeFullDTO where
  toJSON = genericToJSON jsonOptions
