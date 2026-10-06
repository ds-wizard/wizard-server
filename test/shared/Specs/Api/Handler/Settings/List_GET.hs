module Specs.Api.Handler.Settings.List_GET (
  list_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Settings.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Settings.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/settings/{section}
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/settings/{section}" $ do
    test_200_tenant requestContext
    test_200_no_scope requestContext
    test_200_tenant_multi_workspace requestContext
    test_200_workspace requestContext
    test_200_roles_workspace requestContext
    test_200_workspace_role requestContext
    test_400_organization_only requestContext
    test_400_single_workspace requestContext
    test_400_roles_single_workspace requestContext
    test_400_no_scope_multi_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_roles requestContext
    test_403_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_tenant requestContext = do
  create_test_200_tenant requestContext "authentication" settingsAuthentication
  create_test_200_tenant requestContext "users" settingsUsers
  create_test_200_tenant requestContext "roles" settingsRoles
  create_test_200_tenant requestContext "login-screen" settingsLoginScreen
  create_test_200_tenant requestContext "features" settingsFeatures
  create_test_200_tenant requestContext "look-and-feel" settingsLookAndFeel
  create_test_200_tenant requestContext "registry" settingsRegistry
  create_test_200_tenant requestContext "dashboard-and-menu" settingsDashboardAndMenu
  create_test_200_tenant requestContext "projects" settingsProjects
  create_test_200_tenant requestContext "support" settingsSupport
  create_test_200_tenant requestContext "submission" settingsSubmissionEmpty

create_test_200_tenant requestContext section value =
  it ("HTTP 200 OK (" ++ section ++ ", tenant=true)") $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl section) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO value Nothing Nothing) response

test_200_no_scope requestContext =
  it "HTTP 200 OK (no scope in a single-workspace tenant reads the organization value)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (noScopeUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport Nothing Nothing) response

test_200_tenant_multi_workspace requestContext =
  it "HTTP 200 OK (overridable section carries overrideAllowed in a multi-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport (Just True) Nothing) response

test_200_workspace requestContext = do
  it "HTTP 200 OK (w without override returns the organization value)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport (Just True) (Just False)) response
  it "HTTP 200 OK (w with override returns the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (saveSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO editedSettingsSupport (Just True) (Just True)) response

test_200_roles_workspace requestContext =
  it "HTTP 200 OK (roles with w returns the default role of the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "roles") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO (SettingsRoles defaultWorkspaceUserRole.uuid) Nothing Nothing) response

test_200_workspace_role requestContext =
  it "HTTP 200 OK (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (SettingsDTO settingsSupport (Just True) (Just False)) response

test_400_organization_only requestContext =
  it "HTTP 400 BAD REQUEST (w on an organization-only section)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "authentication") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_SETTINGS__ORGANIZATION_ONLY) response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_roles_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (roles, w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "roles") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_no_scope_multi_workspace requestContext =
  it "HTTP 400 BAD REQUEST (no scope in a multi-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (noScopeUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod (tenantUrl "support") [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod (tenantUrl "support") [] reqBody "organizationSettings.manage"
test_403_roles requestContext = createNoPermissionTest requestContext reqMethod (tenantUrl "roles") [] reqBody "roles.manage"

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 403 response
