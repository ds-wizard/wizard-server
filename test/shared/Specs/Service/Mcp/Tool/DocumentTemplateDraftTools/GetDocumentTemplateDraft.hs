module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.GetDocumentTemplateDraft (
  get_document_template_draft,
) where

import Control.Monad (void)
import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_document_template_draft)
-- ------------------------------------------------------------------------
get_document_template_draft :: RequestContext -> SpecWith ((), Application)
get_document_template_draft requestContext =
  describe "POST /api/mcp (get_document_template_draft)" $ do
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

reqBody = toolCallBody "get_document_template_draft" reqArguments

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
      runDraftMigrations requestContext
      runInContextIO (void $ insertFile draftFile) requestContext
      runInContextIO (void $ insertAsset draftAsset) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "uuid" structuredContent `shouldBe` toJSON wizardDocumentTemplateDraft.uuid
      liftIO $ jsonField "readme" structuredContent `shouldBe` object ["thirdPartyContent" .= wizardDocumentTemplateDraft.readme]
      liftIO $ jsonField "formats" structuredContent `shouldBe` toJSON wizardDocumentTemplateDraftFormats
      liftIO $ jsonField "projectUuid" structuredContent `shouldBe` toJSON project1.uuid
      liftIO $ jsonField "formatUuid" structuredContent `shouldBe` toJSON formatJson.uuid
      liftIO $ fmap (jsonField "fileName") (arrayItems (jsonField "files" structuredContent)) `shouldBe` [toJSON draftFile.fileName]
      liftIO $ fmap (jsonField "fileName") (arrayItems (jsonField "assets" structuredContent)) `shouldBe` [toJSON draftAsset.fileName]
      assertToolOutputSchema reqHeaders "get_document_template_draft" structuredContent
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_template_draft" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid])
      -- AND: Prepare expectation
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
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
