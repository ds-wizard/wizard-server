module Specs.Api.Handler.User.PluginSettings.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Model.User.UserPluginSettings
import WizardServer.Database.DAO.User.UserPluginSettingsDAO

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserPluginSettingsInDB requestContext userPluginSettings = do
  userPluginSettingsFromDb <- getOneFromDB (findUserPluginSettingsByUserUuidAndPluginUuid userPluginSettings.userUuid userPluginSettings.pluginUuid) requestContext
  liftIO $ userPluginSettingsFromDb.values `shouldBe` userPluginSettings.values
