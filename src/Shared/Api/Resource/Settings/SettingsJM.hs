module Shared.Api.Resource.Settings.SettingsJM where

import Data.Aeson

import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Model.Settings.Settings
import Shared.Util.Aeson

instance FromJSON a => FromJSON (SettingsDTO a) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON a => ToJSON (SettingsDTO a) where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsAuthentication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAuthentication where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsAuthenticationTwoFactorAuth where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAuthenticationTwoFactorAuth where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsUsers where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsUsers where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsRoles where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsRoles where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsLoginScreen where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsLoginScreen where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsAnnouncement where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsAnnouncement where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsRegistry where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsRegistry where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsFeatures where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsFeatures where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsLookAndFeel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsLookAndFeel where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsDashboardAndMenu where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsDashboardAndMenu where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsCustomMenuLink where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsCustomMenuLink where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsProjects where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjects where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsProjectsVisibility where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsVisibility where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsProjectsSharing where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsSharing where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsProjectsTagging where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsProjectsTagging where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSupport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSupport where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSubmission where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmission where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSubmissionService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionService where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSubmissionServiceSupportedFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceSupportedFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSubmissionServiceRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceRequest where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsSubmissionServiceRequestMultipart where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceRequestMultipart where
  toJSON = genericToJSON jsonOptions

instance FromJSON SettingsAnnouncementLevelType

instance ToJSON SettingsAnnouncementLevelType

instance FromJSON ProjectCreation

instance ToJSON ProjectCreation

instance FromJSON SettingsSubmissionServiceSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsSubmissionServiceSimple where
  toJSON = genericToJSON jsonOptions
