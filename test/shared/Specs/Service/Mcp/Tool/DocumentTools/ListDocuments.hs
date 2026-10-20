module Specs.Service.Mcp.Tool.DocumentTools.ListDocuments (
  list_documents,
) where

import Data.Aeson
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.Document.Document
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_documents)
-- ------------------------------------------------------------------------
list_documents :: RequestContext -> SpecWith ((), Application)
list_documents requestContext =
  describe "POST /api/mcp (list_documents)" $ do
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

reqBody = toolCallBody "list_documents" reqArguments

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
      liftIO $ documentUuidsOf response `shouldBe` [Just (toJSON doc1.uuid)]
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "list_documents" structuredContent
  it "HTTP 200 OK if no project is given (the documents of all projects)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_documents" (object [])
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
      liftIO $ L.sort (documentUuidsOf response) `shouldBe` L.sort (fmap (Just . toJSON . (.uuid)) [doc1, doc2, doc3])
  createIsaacToolErrorTest
    "no project is given and the user has no projects.edit"
    requestContext
    (runDocumentMigrations requestContext)
    []
    (toolCallBody "list_documents" (object []))
    (_ERROR_VALIDATION__MISSING_PERMISSION _PROJECTS_EDIT_ROLE_PERMISSION)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
