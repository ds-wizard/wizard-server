module Specs.Api.Handler.Bootstrap.List_Workspace_GET (
  list_workspace_GET,
) where

import Control.Monad (void)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Bootstrap.BootstrapJM ()
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.TenantPluginSettings
import Shared.Database.Migration.Development.Plugin.Data.WorkspacePluginSettings
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Config.SimpleFeature
import Shared.Model.Error.Error
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.WorkspacePluginSettings
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Tenant
import Shared.Model.Workspace.Workspace
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/bootstrap/workspace
-- ------------------------------------------------------------------------
list_workspace_GET :: RequestContext -> SpecWith ((), Application)
list_workspace_GET requestContext =
  describe "GET /api/bootstrap/workspace" $ do
    test_200_single_workspace requestContext
    test_200_override requestContext
    test_200_workspace_branding requestContext
    test_200_plugins requestContext
    test_400_single_workspace_w requestContext
    test_400_multi_workspace_no_w requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/bootstrap/workspace"

reqUrlW = BS.pack $ "/api/bootstrap/workspace?w=" ++ U.toString defaultWorkspaceUuid

reqHeaders = [reqAuthHeader]

reqBody = ""

expDto =
  WorkspaceBootstrapDTO
    { logo = settingsLookAndFeel.logoUrl
    , primaryColor = settingsLookAndFeel.primaryColor
    , dashboardAndMenu = settingsDashboardAndMenu
    , projects = settingsProjects
    , support = settingsSupport
    , submission = SimpleFeature True
    , disabledPlugins = []
    , pluginSettings = M.empty
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_single_workspace requestContext =
  it "HTTP 200 OK (single-workspace tenant resolves the only workspace)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals (encode expDto)}
    response `shouldRespondWith` responseMatcher

test_200_override requestContext =
  it "HTTP 200 OK (workspace override wins)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (expDto {support = editedSettingsSupport} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (saveSettingsSupport defaultTenant.uuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlW reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_workspace_branding requestContext =
  it "HTTP 200 OK (workspace logo and color win over the organization look and feel)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let brandedWorkspace = defaultWorkspace {logo = Just "https://example.com/logo.png", primaryColor = Just "#D81B60"} :: Workspace
    let expBody = encode (expDto {logo = brandedWorkspace.logo, primaryColor = brandedWorkspace.primaryColor} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO (updateWorkspaceByUuid brandedWorkspace) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Restore the workspace
    void $ runInContextIO (updateWorkspaceByUuid defaultWorkspace) requestContext

test_200_plugins requestContext = do
  it "HTTP 200 OK (organization plugin values, single-workspace tenant ignores workspace rows)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (expDto {pluginSettings = plugin1Dict} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
    runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsDisabled) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (plugin switched off by the workspace)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (expDto {disabledPlugins = [plugin1.uuid]} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
    runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsDisabled) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlW reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (plugin switched off by the workspace hides its override)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (expDto {disabledPlugins = [plugin1.uuid]} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
    runInContextIO (insertWorkspacePluginSettings (defaultWorkspacePluginSettingsOverridden {enabled = False} :: WorkspacePluginSettings)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlW reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (workspace override wins over the organization plugin values)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (expDto {pluginSettings = M.singleton plugin1.uuid plugin1Values2} :: WorkspaceBootstrapDTO)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
    runInContextIO (insertWorkspacePluginSettings defaultWorkspacePluginSettingsOverridden) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlW reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_400_single_workspace_w requestContext =
  it "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlW reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 400, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_400_multi_workspace_no_w requestContext =
  it "HTTP 400 BAD REQUEST (no w in a multi-workspace tenant)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 400, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
