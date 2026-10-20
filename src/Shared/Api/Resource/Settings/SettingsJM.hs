module Shared.Api.Resource.Settings.SettingsJM where

import Data.Aeson

import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Model.Settings.Settings
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON a => FromJSON (SettingsDTO a) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON a => ToJSON (SettingsDTO a) where
  toJSON = genericToJSON jsonOptions

instance JsonSchema a => JsonSchema (SettingsDTO a) where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsAuthentication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAuthentication where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsAuthentication where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsAuthenticationTwoFactorAuth where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAuthenticationTwoFactorAuth where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsAuthenticationTwoFactorAuth where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsUsers where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsUsers where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsUsers where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsRoles where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsRoles where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsRoles where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsLoginScreen where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsLoginScreen where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsLoginScreen where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsAnnouncement where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAnnouncement where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsAnnouncement where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsRegistry where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsRegistry where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsRegistry where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsFeatures where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsFeatures where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsFeatures where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsLookAndFeel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsLookAndFeel where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsLookAndFeel where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsDashboardAndMenu where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsDashboardAndMenu where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsDashboardAndMenu where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsCustomMenuLink where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsCustomMenuLink where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsCustomMenuLink where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsProjects where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjects where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsProjects where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsProjectsVisibility where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsVisibility where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsProjectsVisibility where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsProjectsSharing where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsSharing where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsProjectsSharing where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsProjectsTagging where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsTagging where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsProjectsTagging where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSupport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSupport where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSupport where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSubmission where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmission where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmission where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSubmissionService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionService where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmissionService where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSubmissionServiceSupportedFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceSupportedFormat where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmissionServiceSupportedFormat where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSubmissionServiceRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceRequest where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmissionServiceRequest where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsSubmissionServiceRequestMultipart where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceRequestMultipart where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmissionServiceRequestMultipart where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON SettingsAnnouncementLevelType

instance ToJSON SettingsAnnouncementLevelType

instance JsonSchema SettingsAnnouncementLevelType where
  jsonSchema = genericJsonSchema defaultOptions

instance FromJSON ProjectCreation

instance ToJSON ProjectCreation

instance JsonSchema ProjectCreation where
  jsonSchema = genericJsonSchema defaultOptions

instance FromJSON SettingsSubmissionServiceSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceSimple where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SettingsSubmissionServiceSimple where
  jsonSchema = genericJsonSchema jsonOptions
