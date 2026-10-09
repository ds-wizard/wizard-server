module Shared.Service.Settings.WorkspaceSettingsService where

import Control.Applicative ((<|>))
import qualified Data.UUID as U

import Shared.Api.Resource.Settings.SettingsDTO
import qualified Shared.Database.DAO.Settings.SettingsDashboardAndMenuDAO as DashboardAndMenu
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import qualified Shared.Database.DAO.Settings.SettingsProjectsDAO as Projects
import qualified Shared.Database.DAO.Settings.SettingsSubmissionDAO as Submission
import qualified Shared.Database.DAO.Settings.SettingsSupportDAO as Support
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings
import Shared.Model.Workspace.Workspace
import Shared.Service.Project.ProjectValidation
import Shared.Service.Settings.SettingsAuditLog
import Shared.Service.Settings.SettingsService

dashboardAndMenuSection :: WizardRequestContextC s m => OverridableSettings m SettingsDashboardAndMenu
dashboardAndMenuSection = OverridableSettings "dashboardAndMenu" DashboardAndMenu.table DashboardAndMenu.childTables DashboardAndMenu.findSettingsDashboardAndMenu DashboardAndMenu.saveSettingsDashboardAndMenu (const (return ())) dashboardAndMenuChanges

projectsSection :: WizardRequestContextC s m => OverridableSettings m SettingsProjects
projectsSection = OverridableSettings "projects" Projects.table Projects.childTables Projects.findSettingsProjects Projects.saveSettingsProjects (\settings -> validateProjectTags settings.projectTagging.tags) projectsChanges

supportSection :: WizardRequestContextC s m => OverridableSettings m SettingsSupport
supportSection = OverridableSettings "support" Support.table Support.childTables Support.findSettingsSupport Support.saveSettingsSupport (const (return ())) supportChanges

submissionSection :: WizardRequestContextC s m => OverridableSettings m SettingsSubmission
submissionSection = OverridableSettings "submission" Submission.table Submission.childTables Submission.findSettingsSubmission Submission.saveSettingsSubmission (const (return ())) submissionChanges

getEffectiveSettingsDashboardAndMenu :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsDashboardAndMenu
getEffectiveSettingsDashboardAndMenu = getEffectiveSettings dashboardAndMenuSection

getEffectiveSettingsProjects :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsProjects
getEffectiveSettingsProjects = getEffectiveSettings projectsSection

getEffectiveSettingsSupport :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsSupport
getEffectiveSettingsSupport = getEffectiveSettings supportSection

getEffectiveSettingsSubmission :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsSubmission
getEffectiveSettingsSubmission = getEffectiveSettings submissionSection

getEffectiveLookAndFeel :: WizardRequestContextC s m => Workspace -> m SettingsLookAndFeel
getEffectiveLookAndFeel workspace = do
  lookAndFeel <- getCurrentSettings findSettingsLookAndFeel
  return
    lookAndFeel
      { logoUrl = workspace.logo <|> lookAndFeel.logoUrl
      , primaryColor = workspace.primaryColor <|> lookAndFeel.primaryColor
      }

getSettingsDashboardAndMenu :: WizardRequestContextC s m => m (SettingsDTO SettingsDashboardAndMenu)
getSettingsDashboardAndMenu = getOverridableSettings dashboardAndMenuSection

modifySettingsDashboardAndMenu :: WizardRequestContextC s m => SettingsDTO SettingsDashboardAndMenu -> m (SettingsDTO SettingsDashboardAndMenu)
modifySettingsDashboardAndMenu = modifyOverridableSettings dashboardAndMenuSection

deleteSettingsDashboardAndMenu :: WizardRequestContextC s m => m ()
deleteSettingsDashboardAndMenu = deleteOverridableSettings dashboardAndMenuSection

getSettingsProjects :: WizardRequestContextC s m => m (SettingsDTO SettingsProjects)
getSettingsProjects = getOverridableSettings projectsSection

modifySettingsProjects :: WizardRequestContextC s m => SettingsDTO SettingsProjects -> m (SettingsDTO SettingsProjects)
modifySettingsProjects = modifyOverridableSettings projectsSection

deleteSettingsProjects :: WizardRequestContextC s m => m ()
deleteSettingsProjects = deleteOverridableSettings projectsSection

getSettingsSupport :: WizardRequestContextC s m => m (SettingsDTO SettingsSupport)
getSettingsSupport = getOverridableSettings supportSection

modifySettingsSupport :: WizardRequestContextC s m => SettingsDTO SettingsSupport -> m (SettingsDTO SettingsSupport)
modifySettingsSupport = modifyOverridableSettings supportSection

deleteSettingsSupport :: WizardRequestContextC s m => m ()
deleteSettingsSupport = deleteOverridableSettings supportSection

getSettingsSubmission :: WizardRequestContextC s m => m (SettingsDTO SettingsSubmission)
getSettingsSubmission = getOverridableSettings submissionSection

modifySettingsSubmission :: WizardRequestContextC s m => SettingsDTO SettingsSubmission -> m (SettingsDTO SettingsSubmission)
modifySettingsSubmission = modifyOverridableSettings submissionSection

deleteSettingsSubmission :: WizardRequestContextC s m => m ()
deleteSettingsSubmission = deleteOverridableSettings submissionSection
