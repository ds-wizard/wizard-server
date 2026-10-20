module Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.SaveKnowledgeModelSecret (
  save_knowledge_model_secret,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Localization.Messages.Public
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_knowledge_model_secret)
-- ------------------------------------------------------------------------
save_knowledge_model_secret :: RequestContext -> SpecWith ((), Application)
save_knowledge_model_secret requestContext =
  describe "POST /api/mcp (save_knowledge_model_secret)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["uuid" .= newSecretUuid, "tenant" .= True, "name" .= ("token" :: String), "value" .= ("s3cr3t" :: String)]

reqBody = toolCallBody "save_knowledge_model_secret" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ (valueField "name" structuredContent, valueField "value" structuredContent) `shouldBe` (Just (String "token"), Just (String "********"))
      assertToolOutputSchema reqHeaders "save_knowledge_model_secret" structuredContent
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelSecretsInDB requestContext [kmSecret1 {uuid = newSecretUuid, name = "token", value = "s3cr3t", workspaceUuid = Nothing} :: KnowledgeModelSecret]
  it "HTTP 200 OK if the masked value is sent back (the stored value kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_knowledge_model_secret" (object ["uuid" .= kmSecret1.uuid, "name" .= ("renamed" :: String), "value" .= ("********" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO (insertKnowledgeModelSecret kmSecret1) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "value" =<< structuredContentOf response) `shouldBe` Just (String "********")
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelSecretsInDB requestContext [kmSecret1 {name = "renamed"} :: KnowledgeModelSecret]
  it "HTTP 200 OK if the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare expectation
    do
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
      assertKnowledgeModelSecretsInDB requestContext []

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
