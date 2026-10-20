module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.WriteDocumentTemplateFiles (
  write_document_template_files,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Mcp
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Localization.Messages.Mcp.Public
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
-- POST /api/mcp (write_document_template_files)
-- ------------------------------------------------------------------------
write_document_template_files :: RequestContext -> SpecWith ((), Application)
write_document_template_files requestContext =
  describe "POST /api/mcp (write_document_template_files)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqFiles =
  [ fileWrite "src/new.html.j2" [("content", "new")]
  , fileWrite draftHtml.fileName [("content", "changed")]
  , fileWrite draftCss.fileName [("new_path", "css/default.css")]
  ]

reqArguments = draftArguments ["files" .= reqFiles]

reqBody = toolCallBody "write_document_template_files" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expFiles = [("src/new.html.j2", "new"), (draftHtml.fileName, "changed"), ("css/default.css", draftCss.content), (draftSrc2Css.fileName, draftSrc2Css.content)]
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "write_document_template_files" structuredContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateFilesInDB requestContext wizardDocumentTemplateDraft.uuid expFiles
  it "HTTP 200 OK if the call is repeated (the batch applied once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expFiles = [("src/new.html.j2", "new"), (draftHtml.fileName, "changed"), ("css/default.css", draftCss.content), (draftSrc2Css.fileName, draftSrc2Css.content)]
      -- AND: Run migrations
      runFileMigrations requestContext
      _ <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldSatisfy` (/= Nothing)
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateFilesInDB requestContext wizardDocumentTemplateDraft.uuid expFiles
  it "HTTP 200 OK if a folder is moved (a folder with the same prefix untouched)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_files" (draftArguments ["files" .= [fileWrite "src/" [("new_path", "lib")]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateFileNamesInDB requestContext wizardDocumentTemplateDraft.uuid ["lib/default.html.j2", draftCss.fileName, draftSrc2Css.fileName]
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid ["lib/logo.txt"]
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_files" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "files" .= [fileWrite fileDefaultCss.fileName [("content", "changed")]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__NOT_A_DRAFT (U.toString wizardDocumentTemplate.uuid))
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfDocumentTemplateFileInDB requestContext fileDefaultCss
  it "HTTP 200 OK if a file without content does not exist (tool error, nothing of the batch stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_files" (draftArguments ["files" .= [fileWrite "a.txt" [("content", "a")], fileWrite "missing.txt" [("new_path", "b.txt")]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__FILE_NOT_FOUND "missing.txt")
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findFilesByDocumentTemplateUuidAndFileName wizardDocumentTemplateDraft.uuid "a.txt") requestContext 0
  it "HTTP 200 OK if the content is larger than the limit (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_files" (draftArguments ["files" .= [fileWrite "large.txt" [("content", replicate (mcpMaxFileContentLength + 1) 'x')]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_FILE_CONTENT_LENGTH "large.txt" mcpMaxFileContentLength)
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findFilesByDocumentTemplateUuidAndFileName wizardDocumentTemplateDraft.uuid "large.txt") requestContext 0
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
