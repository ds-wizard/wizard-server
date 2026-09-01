module WizardServer.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Api.Resource.Tenant.Config.TenantConfigChangeSM ()
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Util.Swagger
import WizardServer.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import WizardServer.Api.Resource.Tenant.Config.TenantConfigSM ()

instance ToSchema TenantConfigChangeDTO where
  declareNamedSchema = toSwagger defaultTenantConfigChangeDto

instance ToSchema TenantConfigOrganizationChangeDTO where
  declareNamedSchema = toSwagger defaultOrganizationChangeDto

instance ToSchema TenantConfigAuthenticationChangeDTO where
  declareNamedSchema = toSwagger defaultAuthenticationChangeDto

instance ToSchema TenantConfigPrivacyAndSupportChangeDTO where
  declareNamedSchema = toSwagger defaultPrivacyAndSupportChangeDto

instance ToSchema TenantConfigDashboardAndLoginScreenChangeDTO where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenChangeDto

instance ToSchema TenantConfigRegistryChangeDTO where
  declareNamedSchema = toSwagger defaultRegistryChangeDto

instance ToSchema TenantConfigProjectChangeDTO where
  declareNamedSchema = toSwagger defaultProjectChangeDto

instance ToSchema TenantConfigSubmissionChangeDTO where
  declareNamedSchema = toSwagger defaultSubmission

instance ToSchema TenantConfigSubmissionServiceChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionService

instance ToSchema TenantConfigSubmissionServiceSupportedFormatChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceSupportedFormat

instance ToSchema TenantConfigSubmissionServiceRequestChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequest

instance ToSchema TenantConfigSubmissionServiceRequestMultipartChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequestMultipart

instance ToSchema TenantConfigFeaturesChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequestMultipart
