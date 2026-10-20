module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.ReadDocumentTemplateFiles (
  read_document_template_files,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Mcp
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (read_document_template_files)
-- ------------------------------------------------------------------------
read_document_template_files :: RequestContext -> SpecWith ((), Application)
read_document_template_files requestContext =
  describe "POST /api/mcp (read_document_template_files)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = draftArguments ["paths" .= [draftCss.fileName, draftSrc2Css.fileName, "missing.txt"]]

reqBody = toolCallBody "read_document_template_files" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["files" .= [fileContent draftCss, fileContent draftSrc2Css], "missing" .= ["missing.txt" :: String]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "read_document_template_files" expContent
  it "HTTP 200 OK if the template is released" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "read_document_template_files" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "paths" .= [fileDefaultCss.fileName]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["files" .= [fileContent fileDefaultCss], "missing" .= ([] :: [String])])
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a file is larger than the output limit (continued with the cursor)" $
    -- GIVEN: Prepare request
    do
      let reqArguments = draftArguments ["paths" .= [largeHtml.fileName]]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      runInContextIO (insertFile largeHtml) requestContext
      -- WHEN: Call API
      pages <- requestReadPages reqArguments Nothing 20
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      traverse_ (`shouldRespondWith` responseMatcher) pages
      liftIO $ length pages `shouldSatisfy` (> 1)
      liftIO $ fmap resultSizeOf pages `shouldSatisfy` all (maybe False (<= mcpMaxOutputSize))
      liftIO $ fmap truncatedOf pages `shouldBe` (replicate (length pages - 1) (Just (Bool True)) ++ [Nothing])
      liftIO $ concatMap firstFileContent pages `shouldBe` largeHtml.content
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
