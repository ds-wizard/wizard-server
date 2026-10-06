module Shared.Service.Settings.SettingsAuditLog where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.SettingsAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.Workspace.Workspace
import Shared.Service.AuditLog.AuditLogService

al_updateSettings :: WizardRequestContextC s m => Maybe U.UUID -> String -> AuditLogSettingsChanges -> m ()
al_updateSettings mWorkspaceUuid section changes = whenAuditLogEnabled $ do
  entity <- settingsEntity mWorkspaceUuid
  logAuditLogUpdate settingsUpdate entity changes (AuditLogSettingsUpdateData section Nothing)

al_updateSection :: WizardRequestContextC s m => String -> (a -> a -> AuditLogSettingsChanges) -> a -> a -> m ()
al_updateSection section changesFn old new = al_updateSettings Nothing section (changesFn old new)

al_updateRegistrySettings :: WizardRequestContextC s m => SettingsRegistry -> SettingsRegistry -> m ()
al_updateRegistrySettings old new = whenAuditLogEnabled $ do
  entity <- settingsEntity Nothing
  let apiKeyChanged = old.apiKey /= new.apiKey
  let write = if apiKeyChanged then logAuditLogWithChanges else logAuditLogUpdate
  write settingsUpdate entity (registryChanges old new) (AuditLogSettingsUpdateData "registry" (Just apiKeyChanged))

al_setSettingsOverride :: WizardRequestContextC s m => U.UUID -> String -> Bool -> AuditLogSettingsChanges -> m ()
al_setSettingsOverride workspaceUuid section hadOverride changes = whenAuditLogEnabled $ do
  entity <- settingsEntity (Just workspaceUuid)
  let write = if hadOverride then logAuditLogUpdate else logAuditLogWithChanges
  write settingsOverrideSet entity changes (AuditLogSettingsData section)

al_resetSettingsOverride :: WizardRequestContextC s m => U.UUID -> String -> m ()
al_resetSettingsOverride workspaceUuid section = whenAuditLogEnabled $ do
  entity <- settingsEntity (Just workspaceUuid)
  logAuditLog settingsOverrideReset entity (AuditLogSettingsData section)

al_changeSettingsOverrideAllowed :: WizardRequestContextC s m => String -> Bool -> Bool -> m ()
al_changeSettingsOverrideAllowed section overrideAllowed updatedOverrideAllowed = whenAuditLogEnabled $ do
  entity <- settingsEntity Nothing
  logAuditLogUpdate
    settingsOverrideAllowedChange
    entity
    AuditLogOverrideAllowedChanges {workspaceOverrideAllowed = change overrideAllowed updatedOverrideAllowed}
    (AuditLogSettingsData section)

al_uploadLogo :: WizardRequestContextC s m => Maybe U.UUID -> m ()
al_uploadLogo mWorkspaceUuid = whenAuditLogEnabled $ do
  entity <- settingsEntity mWorkspaceUuid
  logAuditLog settingsLogoUpload entity (AuditLogSettingsData "lookAndFeel")

al_deleteLogo :: WizardRequestContextC s m => Maybe U.UUID -> m ()
al_deleteLogo mWorkspaceUuid = whenAuditLogEnabled $ do
  entity <- settingsEntity mWorkspaceUuid
  logAuditLog settingsLogoDelete entity (AuditLogSettingsData "lookAndFeel")

settingsEntity :: WizardRequestContextC s m => Maybe U.UUID -> m AuditLogEntity
settingsEntity Nothing = do
  tenant <- asks (.tenantUuid') >>= findTenantByUuid
  return $ entityOf tenant.uuid tenant.name
settingsEntity (Just workspaceUuid) = do
  workspace <- findWorkspaceByUuid workspaceUuid
  return $ entityInWorkspace workspace.uuid workspace.uuid workspace.name

authenticationChanges :: SettingsAuthentication -> SettingsAuthentication -> AuditLogSettingsChanges
authenticationChanges old new =
  noSettingsChanges
    { registrationEnabled = change old.registrationEnabled new.registrationEnabled
    , nonAdminLoginEnabled = change old.nonAdminLoginEnabled new.nonAdminLoginEnabled
    , sessionExpiration = change old.sessionExpiration new.sessionExpiration
    , userEmailLinkExpiration = change old.userEmailLinkExpiration new.userEmailLinkExpiration
    , twoFactorAuth = change old.twoFactorAuth new.twoFactorAuth
    }
    :: AuditLogSettingsChanges

usersChanges :: SettingsUsers -> SettingsUsers -> AuditLogSettingsChanges
usersChanges old new = noSettingsChanges {affiliations = change old.affiliations new.affiliations} :: AuditLogSettingsChanges

rolesChanges :: SettingsRoles -> SettingsRoles -> AuditLogSettingsChanges
rolesChanges old new = noSettingsChanges {defaultRoleUuid = change old.defaultRoleUuid new.defaultRoleUuid} :: AuditLogSettingsChanges

loginScreenChanges :: SettingsLoginScreen -> SettingsLoginScreen -> AuditLogSettingsChanges
loginScreenChanges old new =
  noSettingsChanges
    { loginInfo = change old.loginInfo new.loginInfo
    , loginInfoSidebar = change old.loginInfoSidebar new.loginInfoSidebar
    , announcements = change old.announcements new.announcements
    }
    :: AuditLogSettingsChanges

registryChanges :: SettingsRegistry -> SettingsRegistry -> AuditLogSettingsChanges
registryChanges old new = noSettingsChanges {enabled = change old.enabled new.enabled} :: AuditLogSettingsChanges

featuresChanges :: SettingsFeatures -> SettingsFeatures -> AuditLogSettingsChanges
featuresChanges old new =
  noSettingsChanges
    { aiAssistantEnabled = change old.aiAssistantEnabled new.aiAssistantEnabled
    , toursEnabled = change old.toursEnabled new.toursEnabled
    }
    :: AuditLogSettingsChanges

lookAndFeelChanges :: SettingsLookAndFeel -> SettingsLookAndFeel -> AuditLogSettingsChanges
lookAndFeelChanges old new =
  noSettingsChanges
    { appTitle = change old.appTitle new.appTitle
    , logoUrl = change old.logoUrl new.logoUrl
    , primaryColor = change old.primaryColor new.primaryColor
    }
    :: AuditLogSettingsChanges

dashboardAndMenuChanges :: SettingsDashboardAndMenu -> SettingsDashboardAndMenu -> AuditLogSettingsChanges
dashboardAndMenuChanges old new =
  noSettingsChanges
    { customMenuLinks = change old.customMenuLinks new.customMenuLinks
    , announcements = change old.announcements new.announcements
    }
    :: AuditLogSettingsChanges

projectsChanges :: SettingsProjects -> SettingsProjects -> AuditLogSettingsChanges
projectsChanges old new =
  noSettingsChanges
    { projectVisibility = change old.projectVisibility new.projectVisibility
    , projectSharing = change old.projectSharing new.projectSharing
    , projectCreation = change old.projectCreation new.projectCreation
    , projectTagging = change old.projectTagging new.projectTagging
    , summaryReport = change old.summaryReport new.summaryReport
    }
    :: AuditLogSettingsChanges

supportChanges :: SettingsSupport -> SettingsSupport -> AuditLogSettingsChanges
supportChanges old new =
  noSettingsChanges
    { supportEmail = change old.supportEmail new.supportEmail
    , supportSiteName = change old.supportSiteName new.supportSiteName
    , supportSiteUrl = change old.supportSiteUrl new.supportSiteUrl
    , supportSiteIcon = change old.supportSiteIcon new.supportSiteIcon
    }
    :: AuditLogSettingsChanges

submissionChanges :: SettingsSubmission -> SettingsSubmission -> AuditLogSettingsChanges
submissionChanges old new =
  noSettingsChanges
    { enabled = change old.enabled new.enabled
    , services = mapChange (fmap redactHeaders) <$> change old.services new.services
    }
    :: AuditLogSettingsChanges

redactHeaders :: SettingsSubmissionService -> SettingsSubmissionService
redactHeaders service =
  let request = (service.request {headers = M.map (const "***") service.request.headers} :: SettingsSubmissionServiceRequest)
   in (service {request = request} :: SettingsSubmissionService)
