module Specs.Api.Handler.PluginSettings.List_GET (
  list_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Constant.Workspace
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
-- GET /api/plugin-settings
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/plugin-settings" $ do
    test_200_tenant requestContext
    test_200_tenant_no_scope requestContext
    test_200_tenant_multi_workspace requestContext
    test_200_workspace requestContext
    test_200_workspace_disabled requestContext
    test_200_workspace_overridden requestContext
    test_200_workspace_role requestContext
    test_200_workspace_tenant_role_not_member requestContext
    test_400_single_workspace requestContext
    test_400_scope_conflict requestContext
    test_400_scope_required requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqBody = ""

organizationDto :: Plugin -> Maybe Bool -> PluginSettingsListDTO
organizationDto plugin workspaceOverrideAllowed = PluginSettingsListDTO plugin.uuid plugin.enabled workspaceOverrideAllowed Nothing Nothing

workspaceDto :: Plugin -> Bool -> Bool -> PluginSettingsListDTO
workspaceDto plugin enabled overridden = PluginSettingsListDTO plugin.uuid enabled Nothing (Just plugin.workspaceOverrideAllowed) (Just overridden)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext =
  it "HTTP 200 OK (tenant=true in a single-workspace tenant has no workspaceOverrideAllowed)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [organizationDto plugin1 Nothing, organizationDto plugin2 Nothing] response

test_200_tenant_no_scope requestContext =
  it "HTTP 200 OK (no scope in a single-workspace tenant is the organization plane)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod noScopeUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [organizationDto plugin1 Nothing, organizationDto plugin2 Nothing] response

test_200_tenant_multi_workspace requestContext =
  it "HTTP 200 OK (tenant=true in a multi-workspace tenant carries workspaceOverrideAllowed)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod tenantUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [organizationDto plugin1 (Just True), organizationDto plugin2 (Just True)] response

test_200_workspace requestContext =
  it "HTTP 200 OK (w lists only the plugins the organization enabled)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [workspaceDto plugin1 True False] response

test_200_workspace_disabled requestContext =
  it "HTTP 200 OK (w shows the plugin switched off by the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertDisabledInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [workspaceDto plugin1 False False] response

test_200_workspace_overridden requestContext =
  it "HTTP 200 OK (w shows the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOverrideInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [workspaceDto plugin1 True True] response

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [workspaceDto plugin1 True False] response

test_200_workspace_tenant_role_not_member requestContext =
  it "HTTP 200 OK (w on a workspace the organization role is not a member of)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspaceWithoutMember requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrlFor secondWorkspaceUuid) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 [workspaceDto plugin1 True False] response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_scope_conflict requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true and w together)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl <> "&tenant=true") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT) response

test_400_scope_required requestContext =
  it "HTTP 400 BAD REQUEST (no scope in a multi-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod noScopeUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod tenantUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod tenantUrl [] reqBody "organizationSettings.manage"

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod workspaceUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 403 response
