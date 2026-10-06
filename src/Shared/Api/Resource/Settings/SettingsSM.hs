module Shared.Api.Resource.Settings.SettingsSM where

import Data.Swagger

import Shared.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Model.Settings.Settings
import Shared.Model.Settings.SettingsDM
import Shared.Util.Aeson
import Shared.Util.Swagger

instance ToSchema SettingsAuthentication where
  declareNamedSchema = toSwagger defaultSettingsAuthentication

instance ToSchema (SettingsDTO SettingsAuthentication) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsAuthenticationDTO" (SettingsDTO defaultSettingsAuthentication Nothing Nothing)

instance ToSchema SettingsUsers where
  declareNamedSchema = toSwagger defaultSettingsUsers

instance ToSchema (SettingsDTO SettingsUsers) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsUsersDTO" (SettingsDTO defaultSettingsUsers Nothing Nothing)

instance ToSchema SettingsRoles where
  declareNamedSchema = toSwagger defaultSettingsRoles

instance ToSchema (SettingsDTO SettingsRoles) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsRolesDTO" (SettingsDTO defaultSettingsRoles Nothing Nothing)

instance ToSchema SettingsLoginScreen where
  declareNamedSchema = toSwagger defaultSettingsLoginScreen

instance ToSchema (SettingsDTO SettingsLoginScreen) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsLoginScreenDTO" (SettingsDTO defaultSettingsLoginScreen Nothing Nothing)

instance ToSchema SettingsRegistry where
  declareNamedSchema = toSwagger defaultSettingsRegistry

instance ToSchema (SettingsDTO SettingsRegistry) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsRegistryDTO" (SettingsDTO defaultSettingsRegistry Nothing Nothing)

instance ToSchema SettingsFeatures where
  declareNamedSchema = toSwagger defaultSettingsFeatures

instance ToSchema (SettingsDTO SettingsFeatures) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsFeaturesDTO" (SettingsDTO defaultSettingsFeatures Nothing Nothing)

instance ToSchema SettingsLookAndFeel where
  declareNamedSchema = toSwagger defaultSettingsLookAndFeel

instance ToSchema (SettingsDTO SettingsLookAndFeel) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsLookAndFeelDTO" (SettingsDTO defaultSettingsLookAndFeel Nothing Nothing)

instance ToSchema SettingsDashboardAndMenu where
  declareNamedSchema = toSwagger defaultSettingsDashboardAndMenu

instance ToSchema (SettingsDTO SettingsDashboardAndMenu) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsDashboardAndMenuDTO" (SettingsDTO defaultSettingsDashboardAndMenu (Just True) Nothing)

instance ToSchema SettingsProjects where
  declareNamedSchema = toSwagger defaultSettingsProjects

instance ToSchema (SettingsDTO SettingsProjects) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsProjectsDTO" (SettingsDTO defaultSettingsProjects (Just True) Nothing)

instance ToSchema SettingsSupport where
  declareNamedSchema = toSwagger defaultSettingsSupport

instance ToSchema (SettingsDTO SettingsSupport) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsSupportDTO" (SettingsDTO defaultSettingsSupport (Just True) Nothing)

instance ToSchema SettingsSubmission where
  declareNamedSchema = toSwagger defaultSettingsSubmission

instance ToSchema (SettingsDTO SettingsSubmission) where
  declareNamedSchema = toSwaggerWithDtoName "SettingsSubmissionDTO" (SettingsDTO defaultSettingsSubmission (Just True) Nothing)

instance ToSchema SettingsAuthenticationTwoFactorAuth where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsAnnouncement where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsCustomMenuLink where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsProjectsVisibility where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsProjectsSharing where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsProjectsTagging where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsSubmissionService where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsSubmissionServiceSupportedFormat where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsSubmissionServiceRequest where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsSubmissionServiceRequestMultipart where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)

instance ToSchema SettingsAnnouncementLevelType

instance ToSchema ProjectCreation

instance ToSchema SettingsSubmissionServiceSimple where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions jsonOptions)
