module WizardServer.Service.Tenant.PluginSettings.TenantPluginSettingsService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import WizardServer.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import WizardServer.Model.Tenant.PluginSettings.TenantPluginSettings
import WizardServer.Service.Tenant.PluginSettings.TenantPluginSettingsMapper

getPluginSettings :: WizardRequestContextC s m => U.UUID -> m A.Value
getPluginSettings pluginUuid = do
  pluginSettings <- findTenantPluginSettingsByPluginUuid pluginUuid
  return pluginSettings.values

createOrUpdatePluginSettings :: WizardRequestContextC s m => U.UUID -> A.Value -> m A.Value
createOrUpdatePluginSettings pluginUuid reqDto = do
  now <- liftIO getCurrentTime
  mPluginSettings <- findTenantPluginSettingsByPluginUuid' pluginUuid
  case mPluginSettings of
    Just pluginSettings -> do
      let pluginSettingsUpdated = fromChange pluginSettings reqDto now
      updateTenantPluginSettings pluginSettingsUpdated
      return pluginSettingsUpdated.values
    Nothing -> do
      tenantUuid <- asks (.tenantUuid')
      let pluginSettings = fromCreate reqDto pluginUuid tenantUuid now
      insertTenantPluginSettings pluginSettings
      return pluginSettings.values
