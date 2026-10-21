module Specs.Service.Mcp.Tool.TenantContextTools.GetTenantContext (
  get_tenant_context,
) where

import Data.Aeson
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.JSON.JSONSchema (validate)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Api.Resource.Mcp.TenantContext.TenantContextJM ()
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import qualified Shared.Service.Tenant.Usage.UsageService as Usage
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.TenantContextTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_tenant_context)
-- ------------------------------------------------------------------------
get_tenant_context :: RequestContext -> SpecWith ((), Application)
get_tenant_context requestContext =
  describe "POST /api/mcp (get_tenant_context)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object []

reqBody = toolCallBody "get_tenant_context" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      usage <- getOneFromDB (Usage.getUsage defaultTenantUuid) requestContext
      let expContent = toJSON ((albertContext False [defaultWorkspaceContext]) {usage = Just (toJSON usage)} :: TenantContextDTO)
      let expDto = toolResult expContent
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "get_tenant_context" expContent
  it "HTTP 200 OK if the tools are listed (the output schema refuses a usage of another shape)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let invalidContext = toJSON ((albertContext False [defaultWorkspaceContext]) {usage = Just (String "unlimited")} :: TenantContextDTO)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ fmap (isLeft . (`validate` invalidContext)) (tenantContextOutputSchemaOf response) `shouldBe` Just True
  it "HTTP 200 OK if the user has no settings.manage (the usage left out)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let permissions = filter (/= _SETTINGS_MANAGE_ROLE_PERMISSION) userAlbert.role.permissions
      let expDto = toolResult (toJSON ((albertContext False [defaultWorkspaceContext]) {permissions = L.sort . L.nub $ permissions} :: TenantContextDTO))
      -- AND: Run migrations
      removeAlbertPermissions requestContext [_SETTINGS_MANAGE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a workspace is chosen (its effective settings and permissions)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_tenant_context" (object ["workspace_uuid" .= secondWorkspaceUuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      runInContextIO (saveSettingsProjects defaultTenantUuid (Just secondWorkspaceUuid) editedSettingsProjects) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "workspaceUuid" structuredContent `shouldBe` Just (toJSON secondWorkspaceUuid)
      liftIO $ valueField "multiWorkspace" structuredContent `shouldBe` Just (Bool True)
      liftIO $ valueField "permissions" structuredContent `shouldBe` Just (toJSON . L.sort . L.nub $ userAlbert.role.permissions ++ secondWorkspaceUserRole.permissions)
      liftIO $ (valueField "projects" =<< valueField "settings" structuredContent) `shouldBe` Just (toJSON editedSettingsProjects)
      liftIO $ workspaceNames structuredContent `shouldBe` fmap (String . T.pack) [defaultWorkspace.name, secondWorkspace.name]
  it "HTTP 200 OK if tenant: true is given (the organization settings and permissions)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_tenant_context" (object ["tenant" .= True])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      runInContextIO (saveSettingsProjects defaultTenantUuid (Just secondWorkspaceUuid) editedSettingsProjects) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "workspaceUuid" structuredContent `shouldBe` Just Null
      liftIO $ valueField "permissions" structuredContent `shouldBe` Just (toJSON . L.sort . L.nub $ userAlbert.role.permissions)
      liftIO $ (valueField "projects" =<< valueField "settings" structuredContent) `shouldBe` Just (toJSON settingsProjects)
  create_test_200_tool_error
    "HTTP 200 OK if workspace_uuid comes together with tenant: true (tool error)"
    requestContext
    (return ())
    (object ["workspace_uuid" .= secondWorkspaceUuid, "tenant" .= True])
    (localeMessage _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT)
  create_test_200_tool_error
    "HTTP 200 OK if workspace_uuid is given in a single-workspace tenant (tool error)"
    requestContext
    (return ())
    (object ["workspace_uuid" .= defaultWorkspaceUuid])
    (localeMessage _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
  create_test_200_tool_error
    "HTTP 200 OK if the workspace does not exist (tool error)"
    requestContext
    (enableMultiWorkspace requestContext)
    (object ["workspace_uuid" .= unknownUuid])
    (localeMessage $ _ERROR_VALIDATION__ABSENCE "workspace")
  create_test_200_tool_error
    "HTTP 200 OK if the cursor is invalid (tool error)"
    requestContext
    (return ())
    (object ["cursor" .= ("not-a-cursor" :: String)])
    (localeMessage _ERROR_VALIDATION__MCP_INVALID_CURSOR)
  it "HTTP 200 OK if the result is larger than the output limit (continued with the cursor)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expNames = fmap (.name) (defaultWorkspace : manyWorkspaces)
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      traverse_ (\workspace -> runInContextIO (insertWorkspace workspace) requestContext) manyWorkspaces
      -- WHEN: Call API
      pages <- requestTenantContextPages reqArguments 10
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      traverse_ (`shouldRespondWith` responseMatcher) pages
      let contents = mapMaybe structuredContentOf pages
      liftIO $ length contents `shouldSatisfy` (> 1)
      liftIO $ fmap (valueField "truncated") contents `shouldBe` (replicate (length contents - 1) (Just (Bool True)) ++ [Nothing])
      liftIO $ concatMap workspaceNames contents `shouldBe` fmap (String . T.pack) expNames
      traverse_ (assertToolOutputSchema reqHeaders "get_tenant_context") contents

create_test_200_tool_error title requestContext prepare reqArguments expMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_tenant_context" reqArguments
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult expMessage
      -- AND: Run migrations
      prepare
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
