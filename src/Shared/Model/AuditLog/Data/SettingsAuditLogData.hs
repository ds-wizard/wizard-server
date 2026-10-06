module Shared.Model.AuditLog.Data.SettingsAuditLogData where

import Data.Aeson
import Data.Swagger
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Api.Resource.Settings.SettingsSM ()
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings

newtype AuditLogSettingsData = AuditLogSettingsData
  { section :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogSettingsData

instance ToSchema AuditLogSettingsData

data AuditLogSettingsUpdateData = AuditLogSettingsUpdateData
  { section :: String
  , apiKeyChanged :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogSettingsUpdateData where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance ToSchema AuditLogSettingsUpdateData

data AuditLogOverrideAllowedChanges = AuditLogOverrideAllowedChanges
  { workspaceOverrideAllowed :: Maybe (Change Bool)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogOverrideAllowedChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogOverrideAllowedChanges

data AuditLogRegularSynchronization = AuditLogRegularSynchronization
  { enabled :: Bool
  , hour :: Int
  , minute :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogRegularSynchronization

instance ToSchema AuditLogRegularSynchronization

data AuditLogSettingsChanges = AuditLogSettingsChanges
  { registrationEnabled :: Maybe (Change Bool)
  , nonAdminLoginEnabled :: Maybe (Change Bool)
  , sessionExpiration :: Maybe (Change Integer)
  , userEmailLinkExpiration :: Maybe (Change Integer)
  , twoFactorAuth :: Maybe (Change SettingsAuthenticationTwoFactorAuth)
  , affiliations :: Maybe (Change [String])
  , defaultRoleUuid :: Maybe (Change U.UUID)
  , loginInfo :: Maybe (Change (Maybe String))
  , loginInfoSidebar :: Maybe (Change (Maybe String))
  , announcements :: Maybe (Change [SettingsAnnouncement])
  , aiAssistantEnabled :: Maybe (Change Bool)
  , toursEnabled :: Maybe (Change Bool)
  , appTitle :: Maybe (Change (Maybe String))
  , logoUrl :: Maybe (Change (Maybe String))
  , primaryColor :: Maybe (Change (Maybe String))
  , customMenuLinks :: Maybe (Change [SettingsCustomMenuLink])
  , projectVisibility :: Maybe (Change SettingsProjectsVisibility)
  , projectSharing :: Maybe (Change SettingsProjectsSharing)
  , projectCreation :: Maybe (Change ProjectCreation)
  , projectTagging :: Maybe (Change SettingsProjectsTagging)
  , summaryReport :: Maybe (Change SimpleFeature)
  , supportEmail :: Maybe (Change (Maybe String))
  , supportSiteName :: Maybe (Change (Maybe String))
  , supportSiteUrl :: Maybe (Change (Maybe String))
  , supportSiteIcon :: Maybe (Change (Maybe String))
  , enabled :: Maybe (Change Bool)
  , services :: Maybe (Change [SettingsSubmissionService])
  , regularSynchronization :: Maybe (Change AuditLogRegularSynchronization)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogSettingsChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogSettingsChanges

noSettingsChanges :: AuditLogSettingsChanges
noSettingsChanges =
  AuditLogSettingsChanges
    { registrationEnabled = Nothing
    , nonAdminLoginEnabled = Nothing
    , sessionExpiration = Nothing
    , userEmailLinkExpiration = Nothing
    , twoFactorAuth = Nothing
    , affiliations = Nothing
    , defaultRoleUuid = Nothing
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    , announcements = Nothing
    , aiAssistantEnabled = Nothing
    , toursEnabled = Nothing
    , appTitle = Nothing
    , logoUrl = Nothing
    , primaryColor = Nothing
    , customMenuLinks = Nothing
    , projectVisibility = Nothing
    , projectSharing = Nothing
    , projectCreation = Nothing
    , projectTagging = Nothing
    , summaryReport = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    , enabled = Nothing
    , services = Nothing
    , regularSynchronization = Nothing
    }
