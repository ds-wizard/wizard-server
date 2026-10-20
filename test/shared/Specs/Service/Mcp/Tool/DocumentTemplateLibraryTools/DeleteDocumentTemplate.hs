module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.DeleteDocumentTemplate (
  delete_document_template,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_document_template)
-- ------------------------------------------------------------------------
delete_document_template :: RequestContext -> SpecWith ((), Application)
delete_document_template requestContext =
  describe "POST /api/mcp (delete_document_template)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplate.uuid]

reqBody = toolCallBody "delete_document_template" reqArguments

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
      runTemplateMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf (show (createCoordinate wizardDocumentTemplate))) (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "delete_document_template" structuredContent
      -- AND: Compare state in DB with expectation
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid' wizardDocumentTemplate.uuid) requestContext
      liftIO $ isJust templateFromDb `shouldBe` True
  it "HTTP 200 OK if the confirmation token is given (the template deleted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runTemplateMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "delete_document_template" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid' wizardDocumentTemplate.uuid) requestContext
      liftIO $ isNothing templateFromDb `shouldBe` True
  it "HTTP 200 OK if the confirmation token was issued for all versions (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__CONFIRMATION_TOKEN_MISMATCH)
      -- AND: Run migrations
      runTemplateMigrations requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "delete_document_template" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "all_versions" .= True]))
      let reqBody = toolCallBody "delete_document_template" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
