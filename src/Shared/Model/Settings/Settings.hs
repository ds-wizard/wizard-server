module Shared.Model.Settings.Settings where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Config.SimpleFeature
import Shared.Model.Project.Project

data SettingsAuthentication = SettingsAuthentication
  { registrationEnabled :: Bool
  , nonAdminLoginEnabled :: Bool
  , sessionExpiration :: Integer
  , userEmailLinkExpiration :: Integer
  , twoFactorAuth :: SettingsAuthenticationTwoFactorAuth
  }
  deriving (Generic, Eq, Show)

data SettingsAuthenticationTwoFactorAuth = SettingsAuthenticationTwoFactorAuth
  { enabled :: Bool
  , codeLength :: Int
  , expiration :: Int
  }
  deriving (Generic, Eq, Show)

data SettingsUsers = SettingsUsers
  { affiliations :: [String]
  }
  deriving (Generic, Eq, Show)

data SettingsRoles = SettingsRoles
  { defaultRoleUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data SettingsLoginScreen = SettingsLoginScreen
  { loginInfo :: Maybe String
  , loginInfoSidebar :: Maybe String
  , announcements :: [SettingsAnnouncement]
  }
  deriving (Generic, Eq, Show)

data SettingsAnnouncement = SettingsAnnouncement
  { content :: String
  , level :: SettingsAnnouncementLevelType
  }
  deriving (Generic, Eq, Show)

data SettingsAnnouncementLevelType
  = InfoAnnouncementLevelType
  | WarningAnnouncementLevelType
  | CriticalAnnouncementLevelType
  deriving (Generic, Eq, Show, Read)

data SettingsRegistry = SettingsRegistry
  { enabled :: Bool
  , apiKey :: String
  }
  deriving (Generic, Eq, Show)

data SettingsFeatures = SettingsFeatures
  { aiAssistantEnabled :: Bool
  , toursEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data SettingsLookAndFeel = SettingsLookAndFeel
  { appTitle :: Maybe String
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  }
  deriving (Generic, Eq, Show)

data SettingsDashboardAndMenu = SettingsDashboardAndMenu
  { customMenuLinks :: [SettingsCustomMenuLink]
  , announcements :: [SettingsAnnouncement]
  }
  deriving (Generic, Eq, Show)

data SettingsCustomMenuLink = SettingsCustomMenuLink
  { icon :: String
  , title :: String
  , url :: String
  , newWindow :: Bool
  }
  deriving (Generic, Eq, Show)

data SettingsProjects = SettingsProjects
  { projectVisibility :: SettingsProjectsVisibility
  , projectSharing :: SettingsProjectsSharing
  , projectCreation :: ProjectCreation
  , projectTagging :: SettingsProjectsTagging
  , summaryReport :: SimpleFeature
  }
  deriving (Generic, Eq, Show)

data SettingsProjectsVisibility = SettingsProjectsVisibility
  { enabled :: Bool
  , defaultValue :: ProjectVisibility
  }
  deriving (Generic, Eq, Show)

data SettingsProjectsSharing = SettingsProjectsSharing
  { enabled :: Bool
  , defaultValue :: ProjectSharing
  , anonymousEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data ProjectCreation
  = CustomProjectCreation
  | TemplateProjectCreation
  | TemplateAndCustomProjectCreation
  deriving (Generic, Eq, Show, Read)

data SettingsProjectsTagging = SettingsProjectsTagging
  { enabled :: Bool
  , tags :: [String]
  }
  deriving (Generic, Eq, Show)

data SettingsSupport = SettingsSupport
  { supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

data SettingsSubmission = SettingsSubmission
  { enabled :: Bool
  , services :: [SettingsSubmissionService]
  }
  deriving (Generic, Eq, Show)

data SettingsSubmissionService = SettingsSubmissionService
  { sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [SettingsSubmissionServiceSupportedFormat]
  , request :: SettingsSubmissionServiceRequest
  }
  deriving (Generic, Eq, Show)

data SettingsSubmissionServiceSupportedFormat = SettingsSubmissionServiceSupportedFormat
  { id :: String
  , version :: String
  , formatName :: String
  }
  deriving (Generic, Eq, Show)

data SettingsSubmissionServiceRequest = SettingsSubmissionServiceRequest
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: SettingsSubmissionServiceRequestMultipart
  }
  deriving (Generic, Eq, Show)

data SettingsSubmissionServiceRequestMultipart = SettingsSubmissionServiceRequestMultipart
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

data SettingsSubmissionServiceSimple = SettingsSubmissionServiceSimple
  { sId :: String
  , name :: String
  , description :: String
  }
  deriving (Generic, Eq, Show)
