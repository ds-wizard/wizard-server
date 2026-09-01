module Specs.Api.Handler.Tenant.PluginSettings.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import WizardServer.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import WizardServer.Model.Tenant.PluginSettings.TenantPluginSettings

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTenantPluginSettingsInDB requestContext tenantPluginSettings = do
  tenantPluginSettingsFromDb <- getOneFromDB (findTenantPluginSettingsByPluginUuid tenantPluginSettings.pluginUuid) requestContext
  liftIO $ tenantPluginSettingsFromDb.values `shouldBe` tenantPluginSettings.values
