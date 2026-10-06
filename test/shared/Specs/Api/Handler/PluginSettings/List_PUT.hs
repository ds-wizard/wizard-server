module Specs.Api.Handler.PluginSettings.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.WorkspacePluginSettings
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Plugin.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.WorkspacePluginSettings
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.PluginSettings.Common
import Specs.Api.Handler.Settings.Common (expectJson, expectStatus, reqHeaders, useWorkspaceSettingsManager, useWorkspaceUserRole)
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/plugin-settings
-- ------------------------------------------------------------------------
list_PUT :: RequestContext -> SpecWith ((), Application)
list_PUT requestContext =
  describe "PUT /api/plugin-settings" $ do
    test_200_tenant requestContext
    test_200_tenant_override_allowed_ignored requestContext
    test_200_tenant_override_allowed_kept requestContext
    test_200_tenant_override_not_allowed_resets_values requestContext
    test_200_tenant_disable_keeps_workspace_rows requestContext
    test_200_workspace requestContext
    test_200_workspace_keeps_values requestContext
    test_200_workspace_role requestContext
    test_400_workspace_disabled_by_organization requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_workspace requestContext
    test_404_tenant_unknown_plugin requestContext
    test_404_workspace_unknown_plugin requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

changeBody enabled workspaceOverrideAllowed = encode (M.singleton plugin1.uuid (PluginSettingsChangeDTO enabled workspaceOverrideAllowed))

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext =
  it "HTTP 200 OK (tenant=true disables the plugin)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders (encode pluginDict)
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid False Nothing Nothing Nothing, PluginSettingsListDTO plugin2.uuid False Nothing Nothing Nothing] response
    -- AND: Find result in DB and compare with expectation state
    plugin <- getOneFromDB (findPluginByUuid plugin1.uuid) requestContext
    liftIO $ plugin.enabled `shouldBe` False
    assertPluginAuditInDB requestContext "update" (M.fromList [("enabled", "False")])

test_200_tenant_override_allowed_ignored requestContext =
  it "HTTP 200 OK (workspaceOverrideAllowed is ignored in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders (changeBody True (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid True Nothing Nothing Nothing, PluginSettingsListDTO plugin2.uuid False Nothing Nothing Nothing] response
    -- AND: Find result in DB and compare with expectation state
    plugin <- getOneFromDB (findPluginByUuid plugin1.uuid) requestContext
    liftIO $ plugin.workspaceOverrideAllowed `shouldBe` True
    assertPluginAuditInDB requestContext "update" (M.fromList [("enabled", "True")])

test_200_tenant_override_allowed_kept requestContext =
  it "HTTP 200 OK (workspaceOverrideAllowed omitted keeps the stored flag)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (updatePluginWorkspaceOverrideAllowed plugin1.uuid False) requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders (changeBody True Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid True (Just False) Nothing Nothing, PluginSettingsListDTO plugin2.uuid False (Just True) Nothing Nothing] response
    -- AND: Find result in DB and compare with expectation state
    plugin <- getOneFromDB (findPluginByUuid plugin1.uuid) requestContext
    liftIO $ plugin.workspaceOverrideAllowed `shouldBe` False
    assertPluginAuditInDB requestContext "update" (M.fromList [("enabled", "True")])

test_200_tenant_override_not_allowed_resets_values requestContext =
  it "HTTP 200 OK (workspaceOverrideAllowed false resets the values of every workspace and keeps enabled)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    runInContextIO (insertWorkspacePluginSettings (defaultWorkspacePluginSettingsOverridden {enabled = False} :: WorkspacePluginSettings)) requestContext
    runInContextIO (insertWorkspacePluginSettings secondWorkspacePluginSettingsOverridden) requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders (changeBody True (Just False))
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid True (Just False) Nothing Nothing, PluginSettingsListDTO plugin2.uuid False (Just True) Nothing Nothing] response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid False Nothing
    assertWorkspacePluginSettingsInDB requestContext secondWorkspaceUuid plugin1.uuid True Nothing
    assertPluginAuditInDB requestContext "update" (M.fromList [("enabled", "True"), ("workspaceOverrideAllowed", "False")])

test_200_tenant_disable_keeps_workspace_rows requestContext =
  it "HTTP 200 OK (disabling and re-enabling on the organization keeps the workspace rows)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOverrideInWorkspace requestContext
    -- WHEN: Call API
    responseDisable <- request reqMethod tenantUrl reqHeaders (changeBody False Nothing)
    responseEnable <- request reqMethod tenantUrl reqHeaders (changeBody True Nothing)
    -- THEN: Compare response with expectation
    expectStatus 200 responseDisable
    expectStatus 200 responseEnable
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid True (Just plugin1Values2)

test_200_workspace requestContext =
  it "HTTP 200 OK (w switches the plugin off in the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (changeBody False Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid False Nothing (Just True) (Just False)] response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid False Nothing
    plugin <- getOneFromDB (findPluginByUuid plugin1.uuid) requestContext
    liftIO $ plugin.enabled `shouldBe` True
    assertPluginAuditInDB requestContext "updateInWorkspace" (M.fromList [("workspaceUuid", U.toString defaultWorkspaceUuid), ("enabled", "False")])

test_200_workspace_keeps_values requestContext =
  it "HTTP 200 OK (w keeps the override when switching the plugin off)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOverrideInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (changeBody False Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid False Nothing (Just True) (Just True)] response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid False (Just plugin1Values2)

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (changeBody False Nothing)
    -- THEN: Compare response with expectation
    expectJson 200 [PluginSettingsListDTO plugin1.uuid False Nothing (Just True) (Just False)] response

test_400_workspace_disabled_by_organization requestContext =
  it "HTTP 400 BAD REQUEST (w for a plugin the organization has off)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (encode (M.singleton plugin2.uuid (PluginSettingsChangeDTO True Nothing)))
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_PLUGIN__DISABLED_BY_ORGANIZATION) response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (changeBody False Nothing)
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod tenantUrl [reqCtHeader] (changeBody False Nothing)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod tenantUrl [reqCtHeader] (changeBody False Nothing) "organizationSettings.manage"

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (changeBody False Nothing)
    -- THEN: Compare response with expectation
    expectStatus 403 response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404_tenant_unknown_plugin requestContext =
  it "HTTP 404 NOT FOUND (tenant=true for an unknown plugin)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders (encode (M.singleton unknownPlugin.uuid (PluginSettingsChangeDTO True Nothing)))
    -- THEN: Compare response with expectation
    expectStatus 404 response

test_404_workspace_unknown_plugin requestContext =
  it "HTTP 404 NOT FOUND (w for an unknown plugin)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders (encode (M.singleton unknownPlugin.uuid (PluginSettingsChangeDTO True Nothing)))
    -- THEN: Compare response with expectation
    expectStatus 404 response
