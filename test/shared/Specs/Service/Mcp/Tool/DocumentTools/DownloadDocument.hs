module Specs.Service.Mcp.Tool.DocumentTools.DownloadDocument (
  download_document,
) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.Document.Document
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (download_document)
-- ------------------------------------------------------------------------
download_document :: RequestContext -> SpecWith ((), Application)
download_document requestContext =
  describe "POST /api/mcp (download_document)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = documentArguments []

reqBody = toolCallBody "download_document" reqArguments

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
      liftIO $ jsonField "content" structuredContent `shouldBe` object ["thirdPartyContent" .= BS.unpack doc1Content]
      liftIO $ stringField "contentType" structuredContent `shouldBe` Just "text/plain"
      liftIO $ fmap null (stringField "url" structuredContent) `shouldBe` Just False
      liftIO $ jsonField "expiresAt" structuredContent `shouldNotBe` Null
      assertToolOutputSchema reqHeaders "download_document" structuredContent
  it "HTTP 200 OK if the document is not generated yet (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__DOCUMENT_NOT_DONE (U.toString doc1.uuid) (show QueuedDocumentState))
      -- AND: Run migrations
      runDocumentMigrations requestContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) requestContext
      runInContextIO (insertDocument (doc1 {state = QueuedDocumentState})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createIsaacToolErrorTest "the user cannot view the project of the document" requestContext (runDocumentMigrations requestContext) [] reqBody (_ERROR_VALIDATION__FORBIDDEN "View Project")

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
