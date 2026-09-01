module WizardServer.Service.User.PluginSettings.UserPluginSettingsService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserPluginSettings
import WizardServer.Database.DAO.User.UserPluginSettingsDAO
import WizardServer.Service.User.PluginSettings.UserPluginSettingsMapper

getPluginSettings :: WizardRequestContextC s m => U.UUID -> m A.Value
getPluginSettings pluginUuid = do
  user <- getCurrentUser
  pluginSettings <- findUserPluginSettingsByUserUuidAndPluginUuid user.uuid pluginUuid
  return pluginSettings.values

createOrUpdatePluginSettings :: WizardRequestContextC s m => U.UUID -> A.Value -> m A.Value
createOrUpdatePluginSettings pluginUuid reqDto = do
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  mPluginSettings <- findUserPluginSettingsByUserUuidAndPluginUuid' user.uuid pluginUuid
  case mPluginSettings of
    Just pluginSettings -> do
      let pluginSettingsUpdated = fromChange pluginSettings reqDto now
      updateUserPluginSettings pluginSettingsUpdated
      return pluginSettingsUpdated.values
    Nothing -> do
      tenantUuid <- asks (.tenantUuid')
      let pluginSettings = fromCreate reqDto user pluginUuid tenantUuid now
      insertUserPluginSettings pluginSettings
      return pluginSettings.values
