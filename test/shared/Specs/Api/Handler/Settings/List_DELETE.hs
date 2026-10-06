module Specs.Api.Handler.Settings.List_DELETE (
  list_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import qualified Shared.Database.DAO.Settings.SettingsDashboardAndMenuDAO as DashboardAndMenu
import qualified Shared.Database.DAO.Settings.SettingsSubmissionDAO as Submission
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Settings.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/settings/{section}
-- ------------------------------------------------------------------------
list_DELETE :: RequestContext -> SpecWith ((), Application)
list_DELETE requestContext =
  describe "DELETE /api/settings/{section}" $ do
    test_204 requestContext
    test_204_children requestContext
    test_204_workspace_role requestContext
    test_400_tenant requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT (w removes the override)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (saveSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    override <- getOneFromDB (findSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid)) requestContext
    liftIO $ override `shouldBe` Nothing

test_204_children requestContext = do
  create_test_204_children requestContext "submission" (Submission.saveSettingsSubmission defaultTenant.uuid (Just defaultWorkspaceUuid) settingsSubmission) Submission.childTables
  create_test_204_children requestContext "dashboard-and-menu" (DashboardAndMenu.saveSettingsDashboardAndMenu defaultTenant.uuid (Just defaultWorkspaceUuid) settingsDashboardAndMenu) DashboardAndMenu.childTables

create_test_204_children requestContext section saveOverride childTables =
  it ("HTTP 204 NO CONTENT (" ++ section ++ ", w removes the child rows)") $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO saveOverride requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl section) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    counts <- traverse (`countWorkspaceRows` requestContext) childTables
    liftIO $ counts `shouldBe` fmap (const 0) childTables

test_204_workspace_role requestContext =
  it "HTTP 204 NO CONTENT (w with settings.manage on the workspace role)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceSettingsManager requestContext
    runInContextIO (saveSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE) response

test_400_tenant requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (tenantUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod (workspaceUrl "support") [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (w without settings.manage in that workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    useWorkspaceUserRole requestContext
    -- WHEN: Call API
    response <- request reqMethod (workspaceUrl "support") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 403 response
