module Specs.Service.Mcp.Tool.SettingsTools.UpdateSettings (
  update_settings,
) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.Settings.SettingsSupportDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.Settings.Settings
import Shared.Model.User.Role
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
-- POST /api/mcp (update_settings)
-- ------------------------------------------------------------------------
update_settings :: RequestContext -> SpecWith ((), Application)
update_settings requestContext =
  describe "POST /api/mcp (update_settings)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["section" .= ("projects" :: String), "value" .= object ["projectSharing" .= object ["anonymousEnabled" .= True]]]

reqBody = toolCallBody "update_settings" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expSettings = settingsProjects {projectSharing = settingsProjects.projectSharing {anonymousEnabled = True}} :: SettingsProjects
      let expDto = toolResult (toJSON (SettingsDTO expSettings Nothing Nothing))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsProjectsInDB requestContext Nothing (Just expSettings)
  it "HTTP 200 OK if a workspace is given (its override created)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("support" :: String), "workspace_uuid" .= defaultWorkspaceUuid, "value" .= object ["supportEmail" .= ("support@example.com" :: String)]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO editedSettingsSupport (Just True) (Just True)))
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsSupportInDB requestContext (Just defaultWorkspaceUuid) (Just editedSettingsSupport)
      assertSettingsSupportInDB requestContext Nothing (Just settingsSupport)
  it "HTTP 200 OK if the override of a workspace is reset" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("support" :: String), "workspace_uuid" .= defaultWorkspaceUuid, "reset_override" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO settingsSupport (Just True) (Just False)))
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      runInContextIO (saveSettingsSupport defaultTenantUuid (Just defaultWorkspaceUuid) editedSettingsSupport) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsSupportInDB requestContext (Just defaultWorkspaceUuid) Nothing
  it "HTTP 200 OK if a section without overrides is reset (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("features" :: String), "reset_override" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__SETTINGS_NOT_OVERRIDABLE "features")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the default role changes and the user has mcp.sensitive" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("roles" :: String), "value" .= object ["defaultRoleUuid" .= dataStewardRole.uuid]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertSettingsRolesInDB requestContext (SettingsRoles dataStewardRole.uuid)
  it "HTTP 200 OK if the default role changes and the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("roles" :: String), "value" .= object ["defaultRoleUuid" .= adminRole.uuid]])
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
      -- AND: Compare state in DB with expectation
      assertSettingsRolesInDB requestContext settingsRoles
  it "HTTP 200 OK if the section is authentication and the user has mcp.sensitive" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("authentication" :: String), "value" .= object ["registrationEnabled" .= False]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expSettings = settingsAuthentication {registrationEnabled = False}
      let expDto = toolResult (toJSON (SettingsDTO expSettings Nothing Nothing))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsAuthenticationInDB requestContext expSettings
  it "HTTP 200 OK if the section is authentication and the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("authentication" :: String), "value" .= object ["nonAdminLoginEnabled" .= False]])
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
      -- AND: Compare state in DB with expectation
      assertSettingsAuthenticationInDB requestContext settingsAuthentication
  it "HTTP 200 OK if a masked header value is sent back (the stored one kept, a new header stored)" $
    -- GIVEN: Prepare request
    do
      let reqService = toJSON (withHeaders [("Api-Key", "********"), ("X-Extra", "abc")] settingsSubmissionService)
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("submission" :: String), "value" .= object ["services" .= [reqService]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expSettings = settingsSubmission {services = [withHeaders [("Api-Key", "${API Token}"), ("X-Extra", "abc")] settingsSubmissionService]} :: SettingsSubmission
      let expDto = toolResult (toJSON (SettingsDTO (maskedSubmission expSettings) Nothing Nothing))
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsSubmissionInDB requestContext expSettings
  it "HTTP 200 OK if a masked header value comes with a new request url (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqService = toJSON (withRequestUrl "https://collector.example.com" (withHeaders [("Api-Key", "********")] settingsSubmissionService))
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("submission" :: String), "value" .= object ["services" .= [reqService]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult . localeMessage . _ERROR_SERVICE_MCP__MASKED_SECRET_TARGET_CHANGED $ "request url of the submission service " ++ settingsSubmissionService.sId
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsSubmissionInDB requestContext settingsSubmission
  it "HTTP 200 OK if the section is submission and the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("submission" :: String), "value" .= object ["enabled" .= False]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      runInContextIO seedSettingsSubmissionService requestContext
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsSubmissionInDB requestContext settingsSubmission
  it "HTTP 200 OK if the masked registry API key is sent back (the stored one kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("registry" :: String), "value" .= object ["enabled" .= False, "apiKey" .= ("********" :: String)]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (toJSON (SettingsDTO (SettingsRegistry False "********") Nothing Nothing))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertSettingsRegistryInDB requestContext (SettingsRegistry False settingsRegistry.apiKey)
  it "HTTP 200 OK if a value has a wrong type (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["section" .= ("features" :: String), "value" .= object ["toursEnabled" .= ("yes" :: String)]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
  it "HTTP 200 OK if the user has neither organizationSettings.manage nor settings.manage (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_settings" (object ["tenant" .= True, "section" .= ("projects" :: String), "value" .= object []])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      removeAlbertPermissions requestContext [_ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
  create_test_200_tools_list
    "HTTP 200 OK if the user has neither organizationSettings.manage nor settings.manage (the settings tools hidden)"
    requestContext
    [_ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION, _SETTINGS_MANAGE_ROLE_PERMISSION]
    []
  create_test_200_tools_list
    "HTTP 200 OK if the user has settings.manage alone (the settings tools listed)"
    requestContext
    [_ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION]
    settingsToolNames

create_test_200_tools_list title requestContext removedPermissions expToolNames =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      removeAlbertPermissions requestContext removedPermissions
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ filter (`elem` settingsToolNames) (mapMaybe (valueField "name") (toolsOf response)) `shouldBe` expToolNames

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
