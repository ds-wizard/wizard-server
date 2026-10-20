module Specs.Api.Handler.Mcp.List_POST (
  list_POST,
) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.UserToken.ApiKeyCreateJM ()
import Shared.Constant.Mcp
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.Mcp.Data.McpRefreshTokens
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/mcp" $ do
    test_200 requestContext
    test_202 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_405 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqUrlWithToolsets toolsets = BS.pack $ "/api/mcp?toolsets=" ++ toolsets

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = rpcBody "ping" (object [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqUrl reqHeaders reqBody (rpcResult (object []))
  create_test_200
    "HTTP 200 OK if the method is initialize"
    requestContext
    reqUrl
    reqHeaders
    (rpcBody "initialize" (object ["protocolVersion" .= ("2025-06-18" :: String)]))
    (initializeResult "2025-06-18")
  create_test_200
    "HTTP 200 OK if initialize asks for an unsupported protocol version"
    requestContext
    reqUrl
    reqHeaders
    (rpcBody "initialize" (object ["protocolVersion" .= ("2024-11-05" :: String)]))
    (initializeResult "2025-11-25")
  it "HTTP 200 OK if initialize names no confirmed step without a tool" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "initialize" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = initializeResult "2025-11-25"
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      liftIO $ mcpInstructions `shouldNotContain` "deleting a workspace"
  it "HTTP 200 OK if the token is an MCP token" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqMcpAuthHeader, reqCtHeader]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = rpcResult (object [])
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the token is an API key" $
    -- GIVEN: Prepare request
    do
      apiKeyResponse <- request methodPost "/api/api-keys" [reqAuthHeader, reqCtHeader] (encode albertCreateApiKey)
      let Just (String apiKey) = valueField "token" =<< decode (simpleBody apiKeyResponse)
      let reqHeaders = [("Authorization", BS.pack $ "Bearer " ++ T.unpack apiKey), reqCtHeader]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = rpcResult (object [])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  create_test_200
    "HTTP 200 OK if the request id is null"
    requestContext
    reqUrl
    reqHeaders
    (encode $ object ["jsonrpc" .= ("2.0" :: String), "id" .= Null, "method" .= ("ping" :: String)])
    (object ["jsonrpc" .= ("2.0" :: String), "id" .= Null, "result" .= object []])
  it "HTTP 200 OK if the method is tools/list" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let tools = toolsOf response
      liftIO $ fmap (valueField "name") tools `shouldBe` fmap (Just . String) toolNames
      let (tenantContextTool : authoringGuideTool : _) = tools
      liftIO $ fmap (valueField "annotations") [tenantContextTool, authoringGuideTool] `shouldBe` [Just readOnlyAnnotations, Just readOnlyAnnotations]
      liftIO $ propertyNames (valueField "inputSchema" tenantContextTool) `shouldBe` ["cursor", "tenant", "workspace_uuid"]
      liftIO $ propertyNames (valueField "inputSchema" authoringGuideTool) `shouldBe` ["cursor", "topic"]
  it "HTTP 200 OK if a tools/call with a progress token reports no progress (JSON answer)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/call" (object ["name" .= ("get_authoring_guide" :: String), "_meta" .= object ["progressToken" .= (7 :: Int)]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      expResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "get_authoring_guide" (object []))
      let Just expDto = decode (simpleBody expResponse)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  create_test_200
    "HTTP 200 OK if the method is unknown"
    requestContext
    reqUrl
    reqHeaders
    (rpcBody "resources/list" (object []))
    (rpcError (-32601) (localeMessage $ _ERROR_SERVICE_MCP__METHOD_NOT_FOUND "resources/list"))
  create_test_200
    "HTTP 200 OK if the tool is unknown"
    requestContext
    reqUrl
    reqHeaders
    (rpcBody "tools/call" (object ["name" .= ("list_unicorns" :: String)]))
    (rpcError (-32602) (localeMessage $ _ERROR_SERVICE_MCP__UNKNOWN_TOOL "list_unicorns"))
  create_test_200
    "HTTP 200 OK if the request is invalid"
    requestContext
    reqUrl
    reqHeaders
    (encode $ object ["jsonrpc" .= ("2.0" :: String)])
    (object ["jsonrpc" .= ("2.0" :: String), "id" .= Null, "error" .= object ["code" .= (-32600 :: Int), "message" .= localeMessage _ERROR_SERVICE_MCP__INVALID_REQUEST]])
  create_test_200
    "HTTP 200 OK if the tool is outside the toolsets of the query (tool error)"
    requestContext
    (reqUrlWithToolsets "dt")
    reqHeaders
    (toolCallBody "list_knowledge_model_editors" (object []))
    (toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__TOOLSET_NOT_ENABLED "km"))
  it "HTTP 200 OK if the tool is outside the toolsets of the grant (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqMcpAuthHeader, reqCtHeader]
      let reqBody = toolCallBody "list_roles" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__TOOLSET_NOT_ENABLED "admin")
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
      runInContextIO (insertMcpRefreshToken albertMcpRefreshToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  create_test_200_without_write
    "HTTP 200 OK if the user has no mcp.write, before the arguments are checked (tool error)"
    requestContext
    (toolCallBody "save_workspace" (object ["uuid" .= ("not-a-uuid" :: String)]))
  create_test_200_without_write
    "HTTP 200 OK if the user has no mcp.write and the call has no arguments (tool error)"
    requestContext
    (rpcBody "tools/call" (object ["name" .= ("save_workspace" :: String)]))
  create_test_200
    "HTTP 200 OK if a required argument is missing (tool error)"
    requestContext
    reqUrl
    reqHeaders
    (toolCallBody "get_document_template" (object []))
    (toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_MISSING_ARGUMENT "document_template_uuid"))
  create_test_200
    "HTTP 200 OK if an argument is invalid (tool error)"
    requestContext
    reqUrl
    reqHeaders
    (toolCallBody "get_document_template" (object ["document_template_uuid" .= ("not-a-uuid" :: String)]))
    (toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT "document_template_uuid" "invalid UUID"))
  create_test_200
    "HTTP 200 OK if a page is negative (tool error)"
    requestContext
    reqUrl
    reqHeaders
    (toolCallBody "list_roles" (object ["page" .= (-1 :: Int)]))
    (toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT "page" "must not be negative"))
  create_test_200
    "HTTP 200 OK if an argument has a wrong type (tool error)"
    requestContext
    reqUrl
    reqHeaders
    (toolCallBody "list_roles" (object ["q" .= (1 :: Int)]))
    (toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_INVALID_ARGUMENT "q" "expected String, but encountered Number"))

create_test_200 title requestContext reqUrl reqHeaders reqBody expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

create_test_200_without_write title requestContext reqBody =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_WRITE_ROLE_PERMISSION)
      -- AND: Run migrations
      removeAlbertPermissions requestContext [_MCP_WRITE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

toolNames :: [T.Text]
toolNames = ["get_tenant_context", "get_authoring_guide", "describe_knowledge_model_events", "list_knowledge_model_editors", "save_knowledge_model_editor", "delete_knowledge_model_editor", "get_knowledge_model_tree", "edit_knowledge_model", "publish_knowledge_model_editor", "start_knowledge_model_migration", "get_knowledge_model_migration", "resolve_knowledge_model_migration", "cancel_knowledge_model_migration", "test_knowledge_model_integration", "list_knowledge_model_packages", "get_knowledge_model_package", "search_registry_packages", "pull_knowledge_model_package", "update_knowledge_model_package", "delete_knowledge_model_package", "export_knowledge_model_package", "import_knowledge_model_package", "get_knowledge_model_translation", "save_knowledge_model_locale", "delete_knowledge_model_locale", "list_knowledge_model_secrets", "save_knowledge_model_secret", "delete_knowledge_model_secret", "list_integration_prefabs", "list_document_template_drafts", "get_document_template_draft", "save_document_template_draft", "delete_document_template_draft", "read_document_template_files", "write_document_template_files", "read_document_template_assets", "write_document_template_assets", "delete_document_template_files", "get_document_context", "preview_document_template", "publish_document_template_draft", "list_document_templates", "get_document_template", "pull_document_template", "update_document_template", "delete_document_template", "export_document_template", "import_document_template", "get_document_template_translation", "save_document_template_locale", "delete_document_template_locale", "set_knowledge_model_editor_replies", "list_projects", "get_project", "create_project", "clone_project", "get_project_questionnaire", "set_project_replies", "update_project_settings", "set_project_sharing", "migrate_project", "delete_project", "list_documents", "create_document", "download_document", "delete_document", "list_document_submission_services", "submit_document", "list_project_versions", "save_project_version", "delete_project_version", "list_project_events", "get_project_event", "revert_project", "list_project_comment_threads", "create_project_comment_thread", "add_project_comment", "edit_project_comment", "resolve_project_comment_thread", "assign_project_comment_thread", "delete_project_comment_thread", "delete_project_comment", "list_project_files", "upload_project_file", "download_project_file", "delete_project_file", "get_project_report", "get_settings", "update_settings", "list_plugin_settings", "get_plugin_settings", "update_plugin_settings", "list_locales", "get_locale_translation", "save_locale", "delete_locale", "list_mail_template_locales", "get_mail_template_translation", "save_mail_template_locale", "delete_mail_template_locale", "list_workspaces", "list_workspace_members", "save_workspace", "set_workspace_members", "delete_workspace", "list_user_groups", "save_user_group", "delete_user_group", "set_submission_props", "search_users", "update_user", "invite_users", "delete_user", "list_roles", "save_role", "delete_role", "set_user_password", "send_user_email_link", "list_api_keys", "create_api_key", "delete_api_key", "list_mcp_connections", "delete_mcp_connection", "list_open_id_clients", "save_open_id_client", "delete_open_id_client", "list_registry_locales", "pull_registry_locale", "list_tenants", "get_tenant", "save_tenant", "delete_tenant", "update_tenant_limits", "list_dev_operations", "execute_dev_operation", "list_persistent_commands", "get_persistent_command", "update_persistent_command", "retry_persistent_command"]

propertyNames :: Maybe Value -> [KM.Key]
propertyNames (Just (Object schema)) =
  case KM.lookup "properties" schema of
    Just (Object properties) -> KM.keys properties
    _ -> []
propertyNames _ = []

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_202 requestContext =
  it "HTTP 202 ACCEPTED" $
    -- GIVEN: Prepare request
    do
      let reqBody = encode $ object ["jsonrpc" .= ("2.0" :: String), "method" .= ("notifications/initialized" :: String)]
      -- AND: Prepare expectation
      let expStatus = 202
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST if a toolset is unknown" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlWithToolsets "km,unknown"
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext =
  it "HTTP 401 UNAUTHORIZED" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqCtHeader]
      -- AND: Prepare expectation
      let expStatus = 401
      let expHeaders = ("WWW-Authenticate" <:> "Bearer resource_metadata=\"http://localhost:3000/api/mcp/.well-known/oauth-protected-resource\"") : resCtHeader : resCorsHeaders
      let expBody = encode $ UnauthorizedError _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
  it "HTTP 403 FORBIDDEN if the user of an MCP token has no mcp.read" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqMcpAuthHeader, reqCtHeader]
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode . ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_READ_ROLE_PERMISSION
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
      runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _MCP_READ_ROLE_PERMISSION)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_405 requestContext =
  it "HTTP 405 METHOD NOT ALLOWED if the method is GET" $
    -- GIVEN: Prepare request
    do
      let reqMethod = methodGet
      let reqHeaders = [reqAuthHeader]
      let reqBody = ""
      -- AND: Prepare expectation
      let expStatus = 405
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
