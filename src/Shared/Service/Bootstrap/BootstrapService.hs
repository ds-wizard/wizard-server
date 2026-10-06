module Shared.Service.Bootstrap.BootstrapService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.UUID as U

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Service.Bootstrap.BootstrapMapper
import Shared.Service.KnowledgeModel.Metamodel.MigrationService
import Shared.Service.Plugin.PluginEffectiveService
import Shared.Service.Settings.SettingsService
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Workspace.WorkspaceScopeService

getWorkspaceBootstrap :: WizardRequestContextC s m => m WorkspaceBootstrapDTO
getWorkspaceBootstrap = do
  workspaceUuid <- requireWorkspaceScope
  workspace <- findWorkspaceByUuid workspaceUuid
  lookAndFeel <- getCurrentSettings findSettingsLookAndFeel
  dashboardAndMenu <- getEffectiveSettingsDashboardAndMenu (Just workspaceUuid)
  projects <- getEffectiveSettingsProjects (Just workspaceUuid)
  support <- getEffectiveSettingsSupport (Just workspaceUuid)
  submission <- getEffectiveSettingsSubmission (Just workspaceUuid)
  effectivePlugins <- getEffectivePlugins workspaceUuid
  return $ toWorkspaceBootstrapDTO workspace lookAndFeel dashboardAndMenu projects support submission effectivePlugins

getBootstrapPlugins :: WizardRequestContextC s m => U.UUID -> m BootstrapPlugins
getBootstrapPlugins tenantUuid = do
  signedIn <- asks (isJust . (.currentUser'))
  plugins <- findPlugins tenantUuid
  pluginSettings <- findTenantPluginSettingValues tenantUuid
  return $ toBootstrapPlugins signedIn plugins pluginSettings

checkBootstrapTenantState :: WizardRequestContextC s m => Maybe String -> Tenant -> m (Maybe String)
checkBootstrapTenantState mServerUrl tenant =
  case tenant.state of
    NotSeededTenantState -> throwError $ UserError (_ERROR_VALIDATION__NOT_SEEDED_TENANT (fromMaybe "not-provided" mServerUrl))
    PendingHousekeepingTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      mCurrentUser <- asks (.currentUser')
      migrateToLatestMetamodelVersionCommand tenant (fmap (.uuid) mCurrentUser)
      return $ Just housekeepingMessage
    HousekeepingInProgressTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      return $ Just housekeepingMessage
    ReadyForUseTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      return Nothing

housekeepingMessage :: String
housekeepingMessage = "We’re currently upgrading the data to the latest version to enhance your experience"

throwErrorIfTenantIsDisabled :: WizardRequestContextC s m => Maybe String -> Tenant -> m ()
throwErrorIfTenantIsDisabled mServerUrl tenant = unless tenant.enabled (throwError . NotExistsError $ _ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE (fromMaybe "not-provided" mServerUrl))
