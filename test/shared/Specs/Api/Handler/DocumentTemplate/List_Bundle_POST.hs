module Specs.Api.Handler.DocumentTemplate.List_Bundle_POST (
  list_bundle_POST,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/document-templates/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /wizard-api/document-templates/bundle" $ do
    test_201 requestContext
    test_201_used_file_uuids requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/document-templates/bundle"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody (toDocumentTemplateArchive (toBundle anotherWizardDocumentTemplate [] [] []) [])

createMultipartBody :: BSL.ByteString -> BSL.ByteString
createMultipartBody content =
  BSL.concat
    [ BSL.pack $ "--" ++ boundary ++ "\r\n"
    , BSL.pack "Content-Disposition: form-data; name=\"file\"; filename=\"template.zip\"\r\n"
    , BSL.pack "Content-Type: application/zip\r\n\r\n"
    , content
    , BSL.pack "\r\n"
    , BSL.pack $ "--" ++ boundary ++ "--\r\n"
    ]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateSimple)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.name `shouldBe` anotherWizardDocumentTemplate.name
      -- AND: Find result in DB and compare with expectation state
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid resDto.uuid) requestContext
      liftIO $ templateFromDb.templateId `shouldBe` anotherWizardDocumentTemplate.templateId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_used_file_uuids requestContext =
  it "HTTP 201 CREATED when the bundle's file and asset uuids are already used by another template" $
    -- GIVEN: Prepare request
    do
      let bundle = toBundle anotherWizardDocumentTemplate wizardDocumentTemplateFormats [fileDefaultHtml, fileDefaultCss] [assetLogo]
      let reqBody = createMultipartBody (toDocumentTemplateArchive bundle [(assetLogo, assetLogoContent)])
      -- AND: Prepare expectation
      let expStatus = 201
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, _, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateSimple)
      assertResStatus status expStatus
      -- AND: Find result in DB and compare with expectation state
      files <- getOneFromDB (findFilesByDocumentTemplateUuid resDto.uuid) requestContext
      assets <- getOneFromDB (findAssetsByDocumentTemplateUuid resDto.uuid) requestContext
      liftIO $ fmap (.fileName) files `shouldMatchList` [fileDefaultHtml.fileName, fileDefaultCss.fileName]
      liftIO $ fmap (.fileName) assets `shouldBe` [assetLogo.fileName]
      liftIO $ fmap (.uuid) files `shouldNotContain` [fileDefaultHtml.uuid]
      liftIO $ fmap (.uuid) assets `shouldNotBe` [assetLogo.uuid]
      assertCountInDB (findFilesByDocumentTemplateUuid wizardDocumentTemplate.uuid) requestContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody "DocumentTemplatesManageRolePermission"
