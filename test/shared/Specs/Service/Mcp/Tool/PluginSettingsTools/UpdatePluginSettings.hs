module Specs.Service.Mcp.Tool.PluginSettingsTools.UpdatePluginSettings (
  update_plugin_settings,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Database.DAO.Plugin.PluginDAO
import Shared.Database.DAO.Plugin.TenantPluginSettingsDAO
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Plugin.Plugin
import Shared.Model.Plugin.TenantPluginSettings
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.PluginSettingsTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (update_plugin_settings)
-- ------------------------------------------------------------------------
update_plugin_settings :: RequestContext -> SpecWith ((), Application)
update_plugin_settings requestContext =
  describe "POST /api/mcp (update_plugin_settings)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = toolCallBody "update_plugin_settings" (object ["plugin_uuid" .= plugin1.uuid, "enabled" .= False, "values" .= plugin1Values1Edited])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["plugin" .= organizationDto plugin1 {enabled = False}]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runPluginMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "update_plugin_settings" expContent
      -- AND: Compare state in DB with expectation
      plugin <- getOneFromDB (findPluginByUuid plugin1.uuid) requestContext
      liftIO $ plugin.enabled `shouldBe` False
      settings <- getOneFromDB (findTenantPluginSettingsByPluginUuid plugin1.uuid) requestContext
      liftIO $ settings.values `shouldBe` plugin1Values1Edited
  it "HTTP 200 OK if only values are given (the plugin stays enabled)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_plugin_settings" (object ["plugin_uuid" .= plugin1.uuid, "values" .= plugin1Values1Edited])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["plugin" .= organizationDto plugin1])
      -- AND: Run migrations
      runPluginMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      settings <- getOneFromDB (findTenantPluginSettingsByPluginUuid plugin1.uuid) requestContext
      liftIO $ settings.values `shouldBe` plugin1Values1Edited
  it "HTTP 200 OK if reset_override is given for the organization (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_plugin_settings" (object ["plugin_uuid" .= plugin1.uuid, "reset_override" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runPluginMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
