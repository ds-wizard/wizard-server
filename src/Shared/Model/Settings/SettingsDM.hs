module Shared.Model.Settings.SettingsDM where

import qualified Data.UUID as U

import Shared.Model.Config.SimpleFeature
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings

defaultSettingsAuthentication :: SettingsAuthentication
defaultSettingsAuthentication =
  SettingsAuthentication
    { registrationEnabled = True
    , nonAdminLoginEnabled = True
    , sessionExpiration = 14 * 24
    , userEmailLinkExpiration = 14 * 24
    , twoFactorAuth = defaultSettingsAuthenticationTwoFactorAuth
    }

defaultSettingsAuthenticationTwoFactorAuth :: SettingsAuthenticationTwoFactorAuth
defaultSettingsAuthenticationTwoFactorAuth =
  SettingsAuthenticationTwoFactorAuth
    { enabled = False
    , codeLength = 6
    , expiration = 600
    }

defaultSettingsUsers :: SettingsUsers
defaultSettingsUsers = SettingsUsers {affiliations = []}

defaultSettingsRoles :: SettingsRoles
defaultSettingsRoles = SettingsRoles {defaultRoleUuid = U.nil}

defaultSettingsLoginScreen :: SettingsLoginScreen
defaultSettingsLoginScreen =
  SettingsLoginScreen
    { loginInfo = Nothing
    , loginInfoSidebar = Nothing
    , announcements = []
    }

defaultSettingsRegistry :: SettingsRegistry
defaultSettingsRegistry = SettingsRegistry {enabled = False, apiKey = ""}

defaultSettingsFeatures :: SettingsFeatures
defaultSettingsFeatures = SettingsFeatures {aiAssistantEnabled = True, toursEnabled = True}

defaultSettingsLookAndFeel :: SettingsLookAndFeel
defaultSettingsLookAndFeel = SettingsLookAndFeel {appTitle = Nothing, logoUrl = Nothing, primaryColor = Nothing}

defaultSettingsDashboardAndMenu :: SettingsDashboardAndMenu
defaultSettingsDashboardAndMenu = SettingsDashboardAndMenu {customMenuLinks = [], announcements = []}

defaultSettingsProjects :: SettingsProjects
defaultSettingsProjects =
  SettingsProjects
    { projectVisibility = SettingsProjectsVisibility {enabled = True, defaultValue = PrivateProjectVisibility}
    , projectSharing = SettingsProjectsSharing {enabled = True, defaultValue = RestrictedProjectSharing, anonymousEnabled = True}
    , projectCreation = TemplateAndCustomProjectCreation
    , projectTagging = SettingsProjectsTagging {enabled = True, tags = []}
    , summaryReport = SimpleFeature True
    }

defaultSettingsSupport :: SettingsSupport
defaultSettingsSupport =
  SettingsSupport
    { supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }

defaultSettingsSubmission :: SettingsSubmission
defaultSettingsSubmission = SettingsSubmission {enabled = False, services = []}
