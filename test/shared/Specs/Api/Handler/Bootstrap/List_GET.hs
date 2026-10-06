module Specs.Api.Handler.Bootstrap.List_GET (
  list_GET,
) where

import Control.Monad (when)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Plugin.Data.TenantPluginSettings
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Tenant.Tenant
import WizardServer.Api.Resource.Bootstrap.BootstrapJM ()
import WizardServer.Database.DAO.User.UserPluginSettingsDAO
import WizardServer.Model.Bootstrap.BootstrapSettings
import WizardServer.Model.Context.RequestContext
import WizardServer.Service.Bootstrap.BootstrapMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/bootstrap
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/bootstrap" $ do
    test_200 requestContext
    test_200_user_roles requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/bootstrap"

reqBody = ""

bootstrapSettings :: BootstrapSettings
bootstrapSettings =
  BootstrapSettings
    { authentication = settingsAuthentication
    , loginScreen = settingsLoginScreen
    , features = settingsFeatures
    , lookAndFeel = settingsLookAndFeel
    , users = settingsUsers
    , registry = settingsRegistry
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (anonymous)" requestContext [] Nothing defaultTenant
  create_test_200 "HTTP 200 OK (authenticated)" requestContext [reqAuthHeader] (Just userAlbertProfile) defaultTenant
  create_test_200 "HTTP 200 OK (multi-workspace tenant)" requestContext [reqAuthHeader] (Just userAlbertProfile) (defaultTenant {multiWorkspace = True})

create_test_200 title requestContext reqHeaders mUserProfile tenant =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let pluginSettings = maybe M.empty (const plugin1Dict) mUserProfile
      let expDto = toBootstrapDTO requestContext.serverConfig tenant bootstrapSettings [] mUserProfile [] defaultTenantModules (BootstrapPlugins [plugin1List, plugin2List] pluginSettings)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      when tenant.multiWorkspace (enableMultiWorkspace requestContext)
      runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) requestContext
      runInContextIO (insertTenantPluginSettings disabledPluginTenantPluginSettings) requestContext
      runInContextIO (insertUserPluginSettings userAlbertPluginSettings) requestContext
      runInContextIO (insertUserPluginSettings userCharlesPluginSettings) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_user_roles requestContext = do
  create_test_200_user_roles "HTTP 200 OK (user carries organizationRole and workspaceRoles)" requestContext False 1
  create_test_200_user_roles "HTTP 200 OK (workspaceRoles lists every membership)" requestContext True 2

create_test_200_user_roles title requestContext withSecondWorkspace expWorkspaceRoleCount =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      when withSecondWorkspace (insertSecondWorkspace requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqAuthHeader] reqBody
      -- THEN: Compare response with expectation
      let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, Value)
      assertResStatus status expStatus
      let mUser = userObject resBody
      liftIO $ fmap (KM.member "organizationRole") mUser `shouldBe` Just True
      liftIO $ (workspaceRoleCount =<< mUser) `shouldBe` Just expWorkspaceRoleCount

userObject :: Value -> Maybe (KM.KeyMap Value)
userObject (Object body) =
  case KM.lookup "user" body of
    Just (Object user) -> Just user
    _ -> Nothing
userObject _ = Nothing

workspaceRoleCount :: KM.KeyMap Value -> Maybe Int
workspaceRoleCount user =
  case KM.lookup "workspaceRoles" user of
    Just (Object workspaceRoles) -> Just (KM.size workspaceRoles)
    _ -> Nothing
