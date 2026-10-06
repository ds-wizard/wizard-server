module Specs.Api.Handler.PluginSettings.Detail_GET (
  detail_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Plugin.Plugin
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.PluginSettings.Common
import Specs.Api.Handler.Settings.Common (expectJson, expectStatus, reqHeaders, useWorkspaceSettingsManager, useWorkspaceUserRole)
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/plugin-settings/{uuid}" $ do
    test_200_tenant requestContext
    test_200_workspace requestContext
    test_200_workspace_overridden requestContext
    test_200_workspace_override_not_allowed requestContext
    test_200_workspace_role requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_workspace requestContext
    test_404_tenant requestContext
    test_404_workspace_no_values requestContext
    test_404_workspace_disabled_by_organization requestContext
    test_404_workspace_unknown_plugin requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext =
  it "HTTP 200 OK (tenant=true returns the organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 plugin1Values1 response

test_200_workspace requestContext =
  it "HTTP 200 OK (w without override returns the organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO False True plugin1Values1) response

test_200_workspace_overridden requestContext =
  it "HTTP 200 OK (w with override returns the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    insertOverrideInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO True True plugin1Values2) response

test_200_workspace_override_not_allowed requestContext =
  it "HTTP 200 OK (w reports overrideAllowed false)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    enableMultiWorkspace requestContext
    runInContextIO (updatePluginWorkspaceOverrideAllowed plugin1.uuid False) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO False False plugin1Values1) response

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO False True plugin1Values1) response

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
test_401 requestContext = createAuthTest reqMethod (tenantDetailUrl plugin1) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod (tenantDetailUrl plugin1) [] reqBody "organizationSettings.manage"

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
test_404_tenant requestContext =
  it "HTTP 404 NOT FOUND (tenant=true without organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 404 response

test_404_workspace_no_values requestContext =
  it "HTTP 404 NOT FOUND (w without override and without organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 404 response

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
