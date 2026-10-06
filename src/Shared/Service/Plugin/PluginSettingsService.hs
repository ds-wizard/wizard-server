module Shared.Service.Plugin.PluginSettingsService where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Plugin.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Plugin.EffectivePlugin
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings
import Shared.Model.Plugin.WorkspacePluginSettings
import Shared.Service.Plugin.PluginAuditLog
import Shared.Service.Plugin.PluginEffectiveService
import Shared.Service.Plugin.PluginMapper
import Shared.Service.Settings.SettingsService

getPluginSettingsList :: WizardRequestContextC s m => m [PluginSettingsListDTO]
getPluginSettingsList = do
  mWorkspaceUuid <- requireSettingsScope
  checkSettingsPermission mWorkspaceUuid
  listPluginSettings mWorkspaceUuid

modifyPluginSettingsList :: WizardRequestContextC s m => M.Map U.UUID PluginSettingsChangeDTO -> m [PluginSettingsListDTO]
modifyPluginSettingsList reqDto =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    checkSettingsPermission mWorkspaceUuid
    case mWorkspaceUuid of
      Nothing -> forM_ (M.toList reqDto) (uncurry modifyOrganizationPlugin)
      Just workspaceUuid -> forM_ (M.toList reqDto) (uncurry (modifyWorkspacePlugin workspaceUuid))
    listPluginSettings mWorkspaceUuid

getPluginSettings :: WizardRequestContextC s m => U.UUID -> m A.Value
getPluginSettings pluginUuid = do
  mWorkspaceUuid <- requireSettingsScope
  checkSettingsPermission mWorkspaceUuid
  case mWorkspaceUuid of
    Nothing -> (.values) <$> findTenantPluginSettingsByPluginUuid pluginUuid
    Just workspaceUuid -> A.toJSON <$> getWorkspacePluginSettings workspaceUuid pluginUuid

modifyPluginSettings :: WizardRequestContextC s m => U.UUID -> A.Value -> m A.Value
modifyPluginSettings pluginUuid reqDto =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    checkSettingsPermission mWorkspaceUuid
    case mWorkspaceUuid of
      Nothing -> do
        plugin <- findPluginByUuid pluginUuid
        saveOrganizationPluginSettings pluginUuid reqDto
        al_configurePlugin Nothing plugin
        return reqDto
      Just workspaceUuid -> do
        plugin <- getOrganizationEnabledPlugin pluginUuid
        unless plugin.workspaceOverrideAllowed (throwError $ UserError _ERROR_SERVICE_PLUGIN__OVERRIDE_NOT_ALLOWED)
        saveWorkspacePluginValues workspaceUuid pluginUuid reqDto
        al_configurePlugin (Just workspaceUuid) plugin
        A.toJSON <$> getWorkspacePluginSettings workspaceUuid pluginUuid

deletePluginSettings :: WizardRequestContextC s m => U.UUID -> m ()
deletePluginSettings pluginUuid =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    case mWorkspaceUuid of
      Nothing -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED
      Just workspaceUuid -> do
        checkSettingsPermission mWorkspaceUuid
        plugin <- getOrganizationEnabledPlugin pluginUuid
        resetWorkspacePluginValues workspaceUuid pluginUuid
        al_resetPluginOverride workspaceUuid plugin

listPluginSettings :: WizardRequestContextC s m => Maybe U.UUID -> m [PluginSettingsListDTO]
listPluginSettings Nothing = do
  tenantUuid <- asks (.tenantUuid')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  fmap (toOrganizationPluginSettingsListDTO multiWorkspace) <$> findPluginsByTenantUuid tenantUuid
listPluginSettings (Just workspaceUuid) = fmap toWorkspacePluginSettingsListDTO <$> getEffectivePlugins workspaceUuid

modifyOrganizationPlugin :: WizardRequestContextC s m => U.UUID -> PluginSettingsChangeDTO -> m ()
modifyOrganizationPlugin pluginUuid change = do
  plugin <- findPluginByUuid pluginUuid
  void $ updatePluginEnabled pluginUuid change.enabled
  multiWorkspace <- asks (.tenantMultiWorkspace')
  let mWorkspaceOverrideAllowed = if multiWorkspace then change.workspaceOverrideAllowed else Nothing
  forM_ mWorkspaceOverrideAllowed (modifyWorkspaceOverrideAllowed pluginUuid)
  when (plugin.enabled /= change.enabled) (al_enablePlugin Nothing plugin change.enabled)
  forM_ mWorkspaceOverrideAllowed (al_changePluginOverrideAllowed plugin)

modifyWorkspaceOverrideAllowed :: WizardRequestContextC s m => U.UUID -> Bool -> m ()
modifyWorkspaceOverrideAllowed pluginUuid workspaceOverrideAllowed = do
  void $ updatePluginWorkspaceOverrideAllowed pluginUuid workspaceOverrideAllowed
  unless workspaceOverrideAllowed (resetWorkspacePluginValuesByPluginUuid pluginUuid)

modifyWorkspacePlugin :: WizardRequestContextC s m => U.UUID -> U.UUID -> PluginSettingsChangeDTO -> m ()
modifyWorkspacePlugin workspaceUuid pluginUuid change = do
  plugin <- findPluginByUuid pluginUuid
  unless plugin.enabled (throwError $ UserError _ERROR_SERVICE_PLUGIN__DISABLED_BY_ORGANIZATION)
  enabled <- maybe True (.enabled) <$> findWorkspacePluginSettings' workspaceUuid pluginUuid
  saveWorkspacePluginEnabled workspaceUuid pluginUuid change.enabled
  when (enabled /= change.enabled) (al_enablePlugin (Just workspaceUuid) plugin change.enabled)

getWorkspacePluginSettings :: WizardRequestContextC s m => U.UUID -> U.UUID -> m WorkspacePluginSettingsDTO
getWorkspacePluginSettings workspaceUuid pluginUuid = do
  effective <- getEffectivePlugin workspaceUuid pluginUuid
  values <- maybe (throwError . NotExistsError $ _ERROR_VALIDATION__ABSENCE "plugin_settings") return effective.values
  return $ toWorkspacePluginSettingsDTO effective values

saveOrganizationPluginSettings :: WizardRequestContextC s m => U.UUID -> A.Value -> m ()
saveOrganizationPluginSettings pluginUuid values = do
  now <- liftIO getCurrentTime
  mPluginSettings <- findTenantPluginSettingsByPluginUuid' pluginUuid
  case mPluginSettings of
    Just pluginSettings -> void $ updateTenantPluginSettings (pluginSettings {values = values, updatedAt = now} :: TenantPluginSettings)
    Nothing -> do
      tenantUuid <- asks (.tenantUuid')
      void . insertTenantPluginSettings $ toTenantPluginSettings tenantUuid pluginUuid values now
