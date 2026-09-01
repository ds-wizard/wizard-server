module WizardServer.Api.Resource.Tenant.Config.TenantConfigSM where

import Data.Swagger

import Shared.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternSM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Tenant.Config.TenantConfigSM ()
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Util.Swagger

instance ToSchema TenantConfig where
  declareNamedSchema = toSwagger defaultTenantConfig

instance ToSchema TenantConfigOrganization where
  declareNamedSchema = toSwagger defaultOrganization

instance ToSchema TenantConfigAuthentication where
  declareNamedSchema = toSwagger defaultAuthentication

instance ToSchema TenantConfigAuthenticationInternal where
  declareNamedSchema = toSwagger defaultAuthenticationInternal

instance ToSchema TenantConfigAuthenticationInternalTwoFactorAuth where
  declareNamedSchema = toSwagger defaultAuthenticationInternalTwoFactorAuth

instance ToSchema TenantConfigPrivacyAndSupport where
  declareNamedSchema = toSwagger defaultPrivacyAndSupport

instance ToSchema TenantConfigDashboardAndLoginScreen where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreen

instance ToSchema TenantConfigDashboardAndLoginScreenDashboardType

instance ToSchema TenantConfigRegistry where
  declareNamedSchema = toSwagger defaultRegistry

instance ToSchema TenantConfigProject where
  declareNamedSchema = toSwagger defaultProject

instance ToSchema TenantConfigProjectVisibility where
  declareNamedSchema = toSwagger defaultProjectVisibility

instance ToSchema TenantConfigProjectSharing where
  declareNamedSchema = toSwagger defaultProjectSharing

instance ToSchema ProjectCreation

instance ToSchema TenantConfigProjectProjectTagging where
  declareNamedSchema = toSwagger defaultProjectProjectTagging

instance ToSchema TenantConfigSubmission where
  declareNamedSchema = toSwagger defaultSubmission

instance ToSchema TenantConfigSubmissionService where
  declareNamedSchema = toSwagger defaultSubmissionService

instance ToSchema TenantConfigSubmissionServiceSupportedFormat where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceSupportedFormat

instance ToSchema TenantConfigSubmissionServiceRequest where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequest

instance ToSchema TenantConfigSubmissionServiceRequestMultipart where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceRequestMultipart

instance ToSchema TenantConfigOwl where
  declareNamedSchema = toSwagger defaultOwl
