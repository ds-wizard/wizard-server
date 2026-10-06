module Specs.Api.Handler.PluginSettings.Detail_PUT (
  detail_PUT,
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
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.TenantPluginSettings
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Plugin.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.PluginSettings.Common
import Specs.Api.Handler.Settings.Common (expectJson, expectStatus, reqHeaders, useWorkspaceSettingsManager, useWorkspaceUserRole)
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/plugin-settings/{uuid}" $ do
    test_200_tenant requestContext
    test_200_tenant_create requestContext
    test_200_workspace requestContext
    test_200_workspace_keeps_enabled requestContext
    test_200_workspace_role requestContext
    test_400_override_not_allowed requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_workspace requestContext
    test_404_tenant_unknown_plugin requestContext
    test_404_workspace_disabled_by_organization requestContext
    test_404_workspace_unknown_plugin requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqBody = encode plugin1Values1Edited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext =
  it "HTTP 200 OK (tenant=true replaces the organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 plugin1Values1Edited response
    -- AND: Find result in DB and compare with expectation state
    assertTenantPluginSettingsInDB requestContext defaultTenantPluginSettingsEdited
    assertPluginAuditInDB requestContext "updateSettings" M.empty

test_200_tenant_create requestContext =
  it "HTTP 200 OK (tenant=true creates the organization values)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 plugin1Values1Edited response
    -- AND: Find result in DB and compare with expectation state
    assertTenantPluginSettingsInDB requestContext defaultTenantPluginSettingsEdited

test_200_workspace requestContext =
  it "HTTP 200 OK (w creates the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertOrganizationValues requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO True True plugin1Values1Edited) response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid True (Just plugin1Values1Edited)
    assertTenantPluginSettingsInDB requestContext defaultTenantPluginSettings
    assertPluginAuditInDB requestContext "updateSettingsInWorkspace" (M.singleton "workspaceUuid" (U.toString defaultWorkspaceUuid))

test_200_workspace_keeps_enabled requestContext =
  it "HTTP 200 OK (w replaces the override of a plugin switched off in the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertDisabledInWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO True True plugin1Values1Edited) response
    -- AND: Find result in DB and compare with expectation state
    assertWorkspacePluginSettingsInDB requestContext defaultWorkspaceUuid plugin1.uuid False (Just plugin1Values1Edited)

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (WorkspacePluginSettingsDTO True True plugin1Values1Edited) response

test_400_override_not_allowed requestContext =
  it "HTTP 400 BAD REQUEST (w when the organization does not allow the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (updatePluginWorkspaceOverrideAllowed plugin1.uuid False) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceDetailUrl plugin1) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_PLUGIN__OVERRIDE_NOT_ALLOWED) response

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
test_401 requestContext = createAuthTest reqMethod (tenantDetailUrl plugin1) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod (tenantDetailUrl plugin1) [reqCtHeader] reqBody "organizationSettings.manage"

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
test_404_tenant_unknown_plugin requestContext =
  it "HTTP 404 NOT FOUND (tenant=true for an unknown plugin)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantDetailUrl unknownPlugin) reqHeaders reqBody
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
