module Specs.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST (
  list_delete_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Common
import Specs.Api.Handler.DocumentTemplateDraft.Common (runDraftTemplateMigration)
import Specs.Api.Handler.DocumentTemplateDraft.File.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/document-template-drafts/{dtUuid}/folders/delete
-- ------------------------------------------------------------------------
list_delete_POST :: RequestContext -> SpecWith ((), Application)
list_delete_POST requestContext =
  describe "POST /api/document-template-drafts/{dtUuid}/folders/delete" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/folders/delete"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = folderDeleteDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext = create_test_204 "HTTP 201 CREATED (user token)" requestContext reqAuthHeader

create_test_204 title requestContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      -- AND: Prepare folder content
      let siblingFile = fileDefaultCss {uuid = u' "0b4a3f4e-5d0c-4a52-9a43-3f1d7c0b2c11", fileName = "src-other/default.css"} :: DocumentTemplateFile
      let folderAsset = assetLogo {uuid = u' "6f0c1d2e-8b7a-4c3d-9e1f-2a3b4c5d6e7f", fileName = "src/logo.txt"} :: DocumentTemplateAsset
      runDraftTemplateMigration requestContext
      runInContextIO (insertFile siblingFile) requestContext
      runInContextIO (insertAsset folderAsset) requestContext
      runInContextIO (putAsset folderAsset.documentTemplateUuid folderAsset.uuid folderAsset.contentType assetLogoContent) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertAbsenceOfTemplateFileInDB requestContext fileDefaultHtml
      assertExistenceOfTemplateFileInDB requestContext fileDefaultCss
      assertExistenceOfTemplateAssetInDB requestContext assetLogo
      assertExistenceOfTemplateFileInDB requestContext siblingFile
      assertAbsenceOfTemplateAssetInDB requestContext folderAsset
      eContent <- runInContextIO (retrieveAsset folderAsset.documentTemplateUuid folderAsset.uuid) requestContext
      liftIO $ isLeft eContent `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "documentTemplates.useEditor"
