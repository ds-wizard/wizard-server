module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.DeleteDocumentTemplateFiles (
  delete_document_template_files,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
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
-- POST /api/mcp (delete_document_template_files)
-- ------------------------------------------------------------------------
delete_document_template_files :: RequestContext -> SpecWith ((), Application)
delete_document_template_files requestContext =
  describe "POST /api/mcp (delete_document_template_files)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = draftArguments ["paths" .= ["src/" :: String]]

reqBody = toolCallBody "delete_document_template_files" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["deleted" .= [draftHtml.fileName, draftAsset.fileName]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "delete_document_template_files" expContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateFileNamesInDB requestContext wizardDocumentTemplateDraft.uuid [draftCss.fileName, draftSrc2Css.fileName]
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid []
      assertAbsenceOfDocumentTemplateAssetInS3 requestContext draftAsset
  it "HTTP 200 OK if the template has a preview (the preview dropped, a missing path skipped)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_document_template_files" (draftArguments ["paths" .= [draftCss.fileName, "missing.txt"]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["deleted" .= [draftCss.fileName]])
      -- AND: Run migrations
      runPreviewMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB (findFilesByDocumentTemplateUuidAndFileName wizardDocumentTemplateDraft.uuid draftCss.fileName) requestContext 0
      assertCountInDB findDocuments requestContext 0
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_document_template_files" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid, "paths" .= [fileDefaultCss.fileName]])
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
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
