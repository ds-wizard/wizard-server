module Specs.Service.Mcp.Tool.DocumentTemplatePublishTools.PublishDocumentTemplateDraft (
  publish_document_template_draft,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpConfirmation (confirmationToken)
import Shared.Util.Date
import Shared.Util.String (splitOn)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplatePublishTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (publish_document_template_draft)
-- ------------------------------------------------------------------------
publish_document_template_draft :: RequestContext -> SpecWith ((), Application)
publish_document_template_draft requestContext =
  describe "POST /api/mcp (publish_document_template_draft)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplateDraft.uuid]

reqBody = toolCallBody "publish_document_template_draft" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf "global.project-report:2.0.0") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ stringField "confirmation_token" structuredContent `shouldSatisfy` isJust
      assertToolOutputSchema reqHeaders "publish_document_template_draft" structuredContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid DraftDocumentTemplatePhase
  it "HTTP 200 OK if the confirmation token is given (the draft published)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "publish_document_template_draft" (object ["confirmation_token" .= token, "document_template_uuid" .= wizardDocumentTemplateDraft.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Success documentTemplate = fromJSON structuredContent :: Result DocumentTemplateDetailDTO
      liftIO $ documentTemplate.uuid `shouldBe` wizardDocumentTemplateDraft.uuid
      liftIO $ documentTemplate.version `shouldBe` "2.0.0"
      liftIO $ documentTemplate.phase `shouldBe` ReleasedDocumentTemplatePhase
      assertToolOutputSchema reqHeaders "publish_document_template_draft" structuredContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid ReleasedDocumentTemplatePhase
  it "HTTP 200 OK if the version is not higher than the latest released one (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION)
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      runInContextIO (insertDocumentTemplate newerRelease) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid DraftDocumentTemplatePhase
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "publish_document_template_draft" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__NOT_A_DRAFT (show wizardDocumentTemplate.uuid))
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the token was issued before the draft changed (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      runInContextIO (updateDocumentTemplateById wizardDocumentTemplateDraft {readme = "# Changed", updatedAt = dt' 2030 1 1}) requestContext
      let reqBody = toolCallBody "publish_document_template_draft" (object ["confirmation_token" .= token, "document_template_uuid" .= wizardDocumentTemplateDraft.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid DraftDocumentTemplatePhase
  it "HTTP 200 OK if the token expired (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_EXPIRED)
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let [_, nonce, bindingsHash, _] = splitOn "." token
      let expiredToken = confirmationToken requestContext.serverConfig.general.secret 1000000000 nonce bindingsHash
      let reqBody = toolCallBody "publish_document_template_draft" (object ["confirmation_token" .= expiredToken, "document_template_uuid" .= wizardDocumentTemplateDraft.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplateDraft.uuid DraftDocumentTemplatePhase
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
