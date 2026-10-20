module Specs.Service.Mcp.Tool.DocumentTools.CreateDocument (
  create_document,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Document.Document
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (create_document)
-- ------------------------------------------------------------------------
create_document :: RequestContext -> SpecWith ((), Application)
create_document requestContext =
  describe "POST /api/mcp (create_document)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["project_uuid" .= project1.uuid]

reqBody = toolCallBody "create_document" reqArguments

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
      runDocumentMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ stringField "name" structuredContent `shouldBe` Just project1.name
      liftIO $ stringField "state" structuredContent `shouldBe` Just "QueuedDocumentState"
      assertToolOutputSchema reqHeaders "create_document" structuredContent
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 4
  it "HTTP 200 OK if document_uuid is given (the document as it is)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "create_document" (object ["project_uuid" .= project1.uuid, "document_uuid" .= doc1.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runDocumentMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "uuid" structuredContent `shouldBe` toJSON doc1.uuid
      liftIO $ stringField "state" structuredContent `shouldBe` Just "DoneDocumentState"
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 3
  it "HTTP 200 OK if the project has no document template (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "create_document" (object ["project_uuid" .= project1.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP)
      -- AND: Run migrations
      runDocumentMigrations requestContext
      runInContextIO (updateProjectByUuid (project1 {documentTemplateUuid = Nothing, formatUuid = Nothing})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB findDocuments requestContext 3
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
