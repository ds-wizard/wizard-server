module Specs.Api.Handler.PluginSettings.Common where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import Shared.Database.DAO.Audit.AuditDAO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.TenantPluginSettings
import Shared.Database.Migration.Development.Plugin.Data.WorkspacePluginSettings
import Shared.Model.Audit.Audit
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings
import Shared.Model.Plugin.WorkspacePluginSettings
import Shared.Util.Uuid

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

tenantUrl :: BS.ByteString
tenantUrl = "/api/plugin-settings?tenant=true"

noScopeUrl :: BS.ByteString
noScopeUrl = "/api/plugin-settings"

tenantDetailUrl :: Plugin -> BS.ByteString
tenantDetailUrl plugin = BS.pack $ "/api/plugin-settings/" ++ U.toString plugin.uuid ++ "?tenant=true"

workspaceUrl :: BS.ByteString
workspaceUrl = workspaceUrlFor defaultWorkspaceUuid

workspaceUrlFor :: U.UUID -> BS.ByteString
workspaceUrlFor workspaceUuid = BS.pack $ "/api/plugin-settings?w=" ++ U.toString workspaceUuid

workspaceDetailUrl :: Plugin -> BS.ByteString
workspaceDetailUrl plugin = BS.pack $ "/api/plugin-settings/" ++ U.toString plugin.uuid ++ "?w=" ++ U.toString defaultWorkspaceUuid

unknownPlugin :: Plugin
unknownPlugin = plugin1 {uuid = u' "9d0b4c6a-2e7f-4b1d-8c3a-5f6e7a8b9c0d"} :: Plugin

insertOrganizationValues requestContext = do
  runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
  runInContextIO (insertTenantPluginSettings differentTenantPluginSettings) requestContext

insertDisabledInWorkspace requestContext = do
  enableMultiWorkspace requestContext
  runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsDisabled) requestContext

insertOverrideInWorkspace requestContext = do
  enableMultiWorkspace requestContext
  runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsOverridden) requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertTenantPluginSettingsInDB requestContext tenantPluginSettings = do
  tenantPluginSettingsFromDb <- getOneFromDB (findTenantPluginSettingsByPluginUuid tenantPluginSettings.pluginUuid) requestContext
  liftIO $ tenantPluginSettingsFromDb.values `shouldBe` tenantPluginSettings.values

assertWorkspacePluginSettingsInDB requestContext workspaceUuid pluginUuid expEnabled expValues = do
  mRow <- getOneFromDB (findWorkspacePluginSettings' workspaceUuid pluginUuid) requestContext
  liftIO $ fmap (.enabled) mRow `shouldBe` Just expEnabled
  liftIO $ fmap (.values) mRow `shouldBe` Just expValues

assertPluginAuditInDB requestContext expAction expBody = do
  audits <- getOneFromDB findAudits requestContext
  liftIO $ [(audit.action, audit.entity, audit.body) | audit <- audits, audit.component == "plugin"] `shouldBe` [(expAction, U.toString plugin1.uuid, expBody)]
