module Specs.Api.Handler.Config.List_Bootstrap_GET (
  list_bootstrap_GET,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Tenant.Data.TenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Tenant.Tenant
import WizardServer.Api.Resource.Config.ClientConfigJM ()
import WizardServer.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import WizardServer.Database.DAO.User.UserPluginSettingsDAO
import WizardServer.Database.Migration.Development.Tenant.Data.TenantPluginSettings
import WizardServer.Model.Context.RequestContext
import WizardServer.Service.Config.Client.ClientConfigMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: RequestContext -> SpecWith ((), Application)
list_bootstrap_GET requestContext = describe "GET /api/configs/bootstrap" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/configs/bootstrap"

reqHeadersT authHeaders = authHeaders

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (anonymous)" requestContext [] Nothing defaultTenant
  create_test_200 "HTTP 200 OK (authenticated)" requestContext [reqAuthHeader] (Just userAlbertProfile) defaultTenant
  create_test_200 "HTTP 200 OK (multi-workspace tenant)" requestContext [reqAuthHeader] (Just userAlbertProfile) (defaultTenant {multiWorkspace = True})

create_test_200 title requestContext authHeaders mUserProfile tenant =
  it title $
    -- GIVEN: Prepare variables
    do
      let reqHeaders = reqHeadersT authHeaders
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toClientConfigDTO requestContext.serverConfig defaultOrganization defaultAuthentication [] defaultPrivacyAndSupport defaultDashboardAndLoginScreen defaultLookAndFeel defaultRegistry defaultProject defaultSubmission defaultFeatures defaultOwl mUserProfile [] [plugin1List] plugin1Dict defaultTenantModules tenant
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      when tenant.multiWorkspace (enableMultiWorkspace requestContext)
      runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
      runInContextIO (insertTenantPluginSettings differentTenantPluginSettings) requestContext
      runInContextIO (insertUserPluginSettings userAlbertPluginSettings) requestContext
      runInContextIO (insertUserPluginSettings userCharlesPluginSettings) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
