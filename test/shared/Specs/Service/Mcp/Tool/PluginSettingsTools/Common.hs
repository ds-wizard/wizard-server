module Specs.Service.Mcp.Tool.PluginSettingsTools.Common where

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.TenantPluginSettings
import qualified Shared.Database.Migration.Development.Plugin.PluginMigration as Plugin
import Shared.Model.Plugin.Plugin

import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runPluginMigrations requestContext = do
  runInContextIO Plugin.runMigration requestContext
  runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext

organizationDto :: Plugin -> PluginSettingsListDTO
organizationDto plugin = PluginSettingsListDTO plugin.uuid plugin.enabled Nothing Nothing Nothing
