module Specs.Api.Handler.PluginSettings.Detail_DELETE (
  detail_DELETE,
) where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.WorkspacePluginSettings
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.WorkspacePluginSettings
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.PluginSettings.Common
import Specs.Api.Handler.Settings.Common (expectJson, expectStatus, reqHeaders, useWorkspaceSettingsManager, useWorkspaceUserRole)
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/plugin-settings/{uuid}" $ do
    test_204 requestContext
    test_204_keeps_enabled requestContext
    test_204_workspace_role requestContext
    test_400_tenant requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403_workspace requestContext
    test_404_workspace_disabled_by_organization requestContext
    test_404_workspace_unknown_plugin requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT (w resets the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOverrideInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid True Nothing
    assertPluginAuditInDB requestContext "resetSettingsInWorkspace" (M.singleton "workspaceUuid" (U.toString defaultWorkspaceUuid))

test_204_keeps_enabled requestContext =
  it "HTTP 204 NO CONTENT (w keeps the plugin switched off)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (insertWorkspacePluginSettings (defaultWorkspacePluginSettingsOverridden {enabled = False} :: WorkspacePluginSettings)) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid False Nothing

test_204_workspace_role requestContext =
  it "HTTP 204 NO CONTENT (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsOverridden) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response

test_400_tenant requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED) response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod (workspaceDetailUrl plugin1) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 403 response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404_workspace_disabled_by_organization requestContext =
  it "HTTP 404 NOT FOUND (w for a plugin the organization has off)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin2) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 404 response

test_404_workspace_unknown_plugin requestContext =
  it "HTTP 404 NOT FOUND (w for an unknown plugin)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl unknownPlugin) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 404 response
