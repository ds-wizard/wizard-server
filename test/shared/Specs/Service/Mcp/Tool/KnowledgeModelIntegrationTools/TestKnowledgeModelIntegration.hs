module Specs.Service.Mcp.Tool.KnowledgeModelIntegrationTools.TestKnowledgeModelIntegration (
  test_knowledge_model_integration,
) where

import Data.Aeson
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.TypeHint.Data.TypeHints
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Mcp.McpCursor
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Server.McpOutputLimit (encodeCursor)
import Shared.Util.String (replace)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Event.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (test_knowledge_model_integration)
-- ------------------------------------------------------------------------
test_knowledge_model_integration :: RequestContext -> SpecWith ((), Application)
test_knowledge_model_integration requestContext =
  describe "POST /api/mcp (test_knowledge_model_integration)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object (["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid, "integration_uuid" .= repositoryApi.uuid] ++ reqTestArguments)

reqBody = toolCallBody "test_knowledge_model_integration" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (exchangeWithThirdPartyResponse (toJSON repositoryApiTypeHintExchange1))
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "test_knowledge_model_integration" (exchangeWithThirdPartyResponse (toJSON repositoryApiTypeHintExchange1))
  it "HTTP 200 OK if no test values are given (those of the integration used)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["integration_uuid" .= repositoryApi.uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ fmap (L.isSuffixOf ("&q=" ++ repositoryApi.testQ)) (requestUrlOf response) `shouldBe` Just True
  it "HTTP 200 OK if pending events create the integration (called without secrets)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : (["integration_uuid" .= ("$api" :: String), "pending_events" .= [eventChange (U.toString km1.uuid) "$api" (toJSON a_km1_ir.content)]] ++ reqTestArguments)))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expRequest = repositoryApiTypeHintRequest1 {url = "/?domain=biology&country=cz&q=biology", headers = [MapEntry "Api-Key" ""]} :: TypeHintRequest
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "request" =<< structuredContentOf response) `shouldBe` Just (toJSON expRequest)
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if the response contains a secret value (masked)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expResponse = successTypeHintResponse1 {body = replace "Forest Dataset" "[SECRET:mySecret1]" successTypeHintResponse1.body} :: SuccessTypeHintResponse
      let expExchange = repositoryApiTypeHintExchange1 {response = SuccessTypeHintResponse' expResponse}
      let expDto = toolResult . exchangeWithThirdPartyResponse . toJSON $ expExchange
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (insertKnowledgeModelSecret kmSecret1 {value = "Forest Dataset"}) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the integration uses secrets and the user has no mcp.sensitive (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult . localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a cursor is given (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["integration_uuid" .= repositoryApi.uuid, "cursor" .= encodeCursor (McpCursor [KeyMcpCursorStep "response"] 1)]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__MCP_WRITE_TOOL_CURSOR)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a pending edit of the integration changes the request" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : (["integration_uuid" .= repositoryApi.uuid, "pending_events" .= pendingEditEvents] ++ reqTestArguments)))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ fmap (L.isSuffixOf "&extra=1") (requestUrlOf response) `shouldBe` Just True
      -- AND: Compare state in DB with expectation
      assertEventUuidsInDB requestContext amsterdamKnowledgeModelEditorList.uuid amsterdamKnowledgeModelEditorEvents
  it "HTTP 200 OK if the pending events are invalid (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["integration_uuid" .= ("$api" :: String), "pending_events" .= [eventChange "$missing" "$api" (toJSON a_km1_ir.content)]]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
  it "HTTP 200 OK if the integration does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "test_knowledge_model_integration" (object (("knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid) : ["integration_uuid" .= nonExistingUuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__INTEGRATION_ABSENCE)
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the tools are listed (the tool reaches outside the server)" $
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
      let annotations = valueField "annotations" =<< L.find ((== Just (String "test_knowledge_model_integration")) . valueField "name") (toolsOf response)
      liftIO $ annotations `shouldBe` Just (object ["readOnlyHint" .= False, "destructiveHint" .= False, "idempotentHint" .= False, "openWorldHint" .= True])

reqTestArguments = ["variables" .= typeHintTestRequest.variables, "q" .= typeHintTestRequest.q]

pendingEditEvents =
  [ eventChange
      (U.toString km1.uuid)
      (U.toString repositoryApi.uuid)
      (object ["eventType" .= ("EditIntegrationEvent" :: String), "requestUrl" .= object ["changed" .= True, "value" .= (repositoryApi.requestUrl ++ "&extra=1")]])
  ]

exchangeWithThirdPartyResponse :: Value -> Value
exchangeWithThirdPartyResponse exchange =
  case (valueField "request" exchange, valueField "response" exchange) of
    (Just exchangeRequest, Just exchangeResponse) -> object ["request" .= exchangeRequest, "response" .= object ["thirdPartyContent" .= exchangeResponse]]
    _ -> exchange

requestUrlOf response =
  case valueField "url" =<< valueField "request" =<< structuredContentOf response of
    Just (String url) -> Just (T.unpack url)
    _ -> Nothing

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
