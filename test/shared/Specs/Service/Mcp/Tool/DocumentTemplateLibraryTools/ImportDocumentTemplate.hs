module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ImportDocumentTemplate (
  import_document_template,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common (base64Content)

-- ------------------------------------------------------------------------
-- POST /api/mcp (import_document_template)
-- ------------------------------------------------------------------------
import_document_template :: RequestContext -> SpecWith ((), Application)
import_document_template requestContext =
  describe "POST /api/mcp (import_document_template)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["tenant" .= True, "content" .= base64Content (toDocumentTemplateArchive (toBundle anotherWizardDocumentTemplate [] [] []) [])]

reqBody = toolCallBody "import_document_template" reqArguments

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
      liftIO $ stringField "name" structuredContent `shouldBe` Just anotherWizardDocumentTemplate.name
      assertToolOutputSchema reqHeaders "import_document_template" structuredContent
      -- AND: Compare state in DB with expectation
      let Just uuid = U.fromString =<< stringField "uuid" structuredContent
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid uuid) requestContext
      liftIO $ templateFromDb.id `shouldBe` anotherWizardDocumentTemplate.id
      liftIO $ templateFromDb.workspaceUuid `shouldBe` Nothing
  it "HTTP 200 OK if the content is not base64 (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "import_document_template" (object ["tenant" .= True, "content" .= ("not base64!" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_BASE64 "content")
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
