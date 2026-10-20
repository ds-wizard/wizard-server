module Specs.Service.Mcp.Tool.SettingsTools.GetSettings (
  get_settings,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Settings.Settings
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.SettingsTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_settings)
-- ------------------------------------------------------------------------
get_settings :: RequestContext -> SpecWith ((), Application)
get_settings requestContext =
  describe "POST /api/mcp (get_settings)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["section" .= ("projects" :: String)]

reqBody = toolCallBody "get_settings" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON (SettingsDTO settingsProjects Nothing Nothing)
      let expDto = toolResult expContent
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_settings" expContent
  it "HTTP 200 OK if a workspace is given (its override)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("projects" :: String), "workspace_uuid" .= defaultWorkspaceUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO editedSettingsProjects (Just True) (Just True)))
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      runInContextIO (saveSettingsProjects defaultTenantUuid (Just defaultWorkspaceUuid) editedSettingsProjects) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if no scope is given in a multi-workspace tenant (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED)
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the section is submission (the header values masked)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("submission" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON (SettingsDTO (maskedSubmission settingsSubmission) Nothing Nothing)
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_settings" expContent
  it "HTTP 200 OK if the section is registry (the API key masked)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("registry" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO (settingsRegistry {apiKey = "********"}) Nothing Nothing))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the section is authentication and the user has mcp.sensitive" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("authentication" :: String), "tenant" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO settingsAuthentication Nothing Nothing))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the section is authentication and the user has no mcp.sensitive (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("authentication" :: String), "tenant" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the section is unknown (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_settings" (object ["section" .= ("unicorns" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_SETTINGS_SECTION "unicorns" sectionNames)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
