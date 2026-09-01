module Shared.Database.Migration.Development.Tenant.Data.TenantConfigs where

import Shared.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Shared.Constant.Tenant
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Service.Tenant.Config.ConfigMapper
import Shared.Util.Date

defaultDashboardAndLoginScreenAnnouncement :: TenantConfigDashboardAndLoginScreenAnnouncement
defaultDashboardAndLoginScreenAnnouncement = fromDashboardAndLoginScreenAnnouncementChangeDTO defaultDashboardAndLoginScreenAnnouncementChangeDto defaultTenantUuid 0 (dt' 2018 1 20) (dt' 2018 1 20)

defaultDashboardAndLoginScreenAnnouncementChangeDto :: TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO
defaultDashboardAndLoginScreenAnnouncementChangeDto =
  TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO
    { content = "Hello"
    , level = InfoAnnouncementLevelType
    , dashboard = True
    , loginScreen = True
    }

defaultLookAndFeel :: TenantConfigLookAndFeel
defaultLookAndFeel = fromLookAndFeelChangeDTO defaultLookAndFeelChangeDto defaultTenantUuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultLookAndFeelChangeDto :: TenantConfigLookAndFeelChangeDTO
defaultLookAndFeelChangeDto =
  TenantConfigLookAndFeelChangeDTO
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = [defaultLookAndFeelCustomLinkChangeDto]
    , logoUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    }

defaultLookAndFeelCustomLink :: TenantConfigLookAndFeelCustomMenuLink
defaultLookAndFeelCustomLink = fromLookAndFeelCustomMenuLinkChangeDTO defaultLookAndFeelCustomLinkChangeDto defaultTenantUuid 0 (dt' 2018 1 20) (dt' 2018 1 20)

defaultLookAndFeelCustomLinkChangeDto :: TenantConfigLookAndFeelCustomMenuLinkChangeDTO
defaultLookAndFeelCustomLinkChangeDto =
  TenantConfigLookAndFeelCustomMenuLinkChangeDTO
    { icon = "faq"
    , title = "My Link"
    , url = "http://example.prg"
    , newWindow = False
    }

defaultFeatures :: TenantConfigFeatures
defaultFeatures = fromFeaturesChangeFullDTO defaultFeaturesChangeFullDto defaultTenantUuid (dt' 2018 1 20) (dt' 2018 1 20)

defaultFeaturesChangeFullDto :: TenantConfigFeaturesChangeFullDTO
defaultFeaturesChangeFullDto =
  TenantConfigFeaturesChangeFullDTO
    { aiAssistantEnabled = True
    , toursEnabled = True
    }

defaultMail :: TenantConfigMail
defaultMail =
  TenantConfigMail
    { tenantUuid = defaultTenantUuid
    , configUuid = Nothing
    , customTemplates = False
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }
