module Shared.Service.Tenant.Config.WizardConfigMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Tenant.Config.ConfigMapper

toChangeDTO
  :: TenantConfigOrganizationChangeDTO
  -> TenantConfigAuthenticationChangeDTO
  -> TenantConfigPrivacyAndSupportChangeDTO
  -> TenantConfigDashboardAndLoginScreenChangeDTO
  -> TenantConfigLookAndFeelChangeDTO
  -> TenantConfigRegistryChangeDTO
  -> TenantConfigProjectChangeDTO
  -> TenantConfigSubmissionChangeDTO
  -> TenantConfigFeaturesChangeDTO
  -> TenantConfigChangeDTO
toChangeDTO organization authentication privacyAndSupport dashboardAndLoginScreen lookAndFeel registry project submission features = TenantConfigChangeDTO {..}

toSubmissionServiceSimple :: TenantConfigSubmissionService -> TenantConfigSubmissionServiceSimple
toSubmissionServiceSimple config =
  TenantConfigSubmissionServiceSimple
    { sId = config.sId
    , name = config.name
    , description = config.description
    }

toTenantConfig
  :: TenantConfigOrganization
  -> TenantConfigAuthentication
  -> TenantConfigPrivacyAndSupport
  -> TenantConfigDashboardAndLoginScreen
  -> TenantConfigLookAndFeel
  -> TenantConfigRegistry
  -> TenantConfigProject
  -> TenantConfigSubmission
  -> TenantConfigFeatures
  -> TenantConfigOwl
  -> TenantConfig
toTenantConfig organization authentication privacyAndSupport dashboardAndLoginScreen lookAndFeel registry project submission features owl =
  let uuid = organization.tenantUuid
      mailConfigUuid = Nothing
      createdAt = organization.createdAt
      updatedAt = organization.updatedAt
   in TenantConfig {..}

fromOrganizationChangeDTO :: TenantConfigOrganizationChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigOrganization
fromOrganizationChangeDTO TenantConfigOrganizationChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigOrganization {..}

fromAuthenticationChangeDTO :: TenantConfigAuthenticationChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigAuthentication
fromAuthenticationChangeDTO TenantConfigAuthenticationChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigAuthentication {..}

fromPrivacyAndSupportChangeDTO :: TenantConfigPrivacyAndSupportChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigPrivacyAndSupport
fromPrivacyAndSupportChangeDTO TenantConfigPrivacyAndSupportChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigPrivacyAndSupport {..}

fromDashboardAndLoginScreenChangeDTO :: TenantConfigDashboardAndLoginScreenChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigDashboardAndLoginScreen
fromDashboardAndLoginScreenChangeDTO a@TenantConfigDashboardAndLoginScreenChangeDTO {..} tenantUuid createdAt updatedAt =
  let announcements = zipWith (\i c -> fromDashboardAndLoginScreenAnnouncementChangeDTO c tenantUuid i createdAt updatedAt) [0 ..] a.announcements
   in TenantConfigDashboardAndLoginScreen {..}

fromRegistryChangeDTO :: TenantConfigRegistryChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigRegistry
fromRegistryChangeDTO TenantConfigRegistryChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigRegistry {..}

fromProjectChangeDTO :: TenantConfigProjectChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigProject
fromProjectChangeDTO TenantConfigProjectChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigProject {..}

fromSubmissionChangeDTO :: TenantConfigSubmissionChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigSubmission
fromSubmissionChangeDTO dto@TenantConfigSubmissionChangeDTO {..} tenantUuid createdAt updatedAt =
  let services = fmap (\s -> fromSubmissionServiceChangeDTO s tenantUuid createdAt updatedAt) dto.services
   in TenantConfigSubmission {..}

fromSubmissionServiceChangeDTO :: TenantConfigSubmissionServiceChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigSubmissionService
fromSubmissionServiceChangeDTO dto@TenantConfigSubmissionServiceChangeDTO {..} tenantUuid createdAt updatedAt =
  let supportedFormats = fmap (\f -> fromSubmissionServiceSupportedFormatChangeDTO f tenantUuid dto.sId) dto.supportedFormats
      request = fromSubmissionServiceRequestChangeDTO dto.request
   in TenantConfigSubmissionService {..}

fromSubmissionServiceSupportedFormatChangeDTO :: TenantConfigSubmissionServiceSupportedFormatChangeDTO -> U.UUID -> String -> TenantConfigSubmissionServiceSupportedFormat
fromSubmissionServiceSupportedFormatChangeDTO TenantConfigSubmissionServiceSupportedFormatChangeDTO {..} tenantUuid serviceId = TenantConfigSubmissionServiceSupportedFormat {..}

fromSubmissionServiceRequestChangeDTO :: TenantConfigSubmissionServiceRequestChangeDTO -> TenantConfigSubmissionServiceRequest
fromSubmissionServiceRequestChangeDTO dto@TenantConfigSubmissionServiceRequestChangeDTO {..} =
  let multipart = fromSubmissionServiceRequestMultipartChangeDTO dto.multipart
   in TenantConfigSubmissionServiceRequest {..}

fromSubmissionServiceRequestMultipartChangeDTO :: TenantConfigSubmissionServiceRequestMultipartChangeDTO -> TenantConfigSubmissionServiceRequestMultipart
fromSubmissionServiceRequestMultipartChangeDTO TenantConfigSubmissionServiceRequestMultipartChangeDTO {..} =
  TenantConfigSubmissionServiceRequestMultipart {..}

fromFeaturesChangeDTO :: TenantConfigFeaturesChangeDTO -> TenantConfigFeatures -> U.UUID -> UTCTime -> UTCTime -> TenantConfigFeatures
fromFeaturesChangeDTO dto oldConfig tenantUuid createdAt updatedAt =
  let aiAssistantEnabled = oldConfig.aiAssistantEnabled
      toursEnabled = dto.toursEnabled
   in TenantConfigFeatures {..}
