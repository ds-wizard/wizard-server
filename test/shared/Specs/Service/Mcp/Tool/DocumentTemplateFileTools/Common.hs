module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.Common where

import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as Project
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runFileMigrations requestContext = do
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO (traverse_ insertFile [draftHtml, draftCss, draftSrc2Css]) requestContext
  runInContextIO (void $ insertAsset draftAsset) requestContext
  runInContextIO (void $ putAsset draftAsset.documentTemplateUuid draftAsset.uuid draftAsset.contentType assetLogoContent) requestContext

runPreviewMigrations requestContext = do
  runInContextIO User.runMigration requestContext
  runFileMigrations requestContext
  runInContextIO Project.runMigration requestContext
  runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
  runInContextIO deleteDocuments requestContext
  runInContextIO (insertDocument (tempDocDone {documentTemplateUuid = wizardDocumentTemplateDraft.uuid} :: Document)) requestContext

draftArguments :: [(K.Key, Value)] -> Value
draftArguments arguments = object (("document_template_uuid" .= wizardDocumentTemplateDraft.uuid) : arguments)

fileWrite :: String -> [(String, String)] -> Value
fileWrite path fields = object (("path" .= path) : fmap (\(name, value) -> K.fromString name .= value) fields)

fileContent :: DocumentTemplateFile -> Value
fileContent file = object ["path" .= file.fileName, "content" .= object ["thirdPartyContent" .= file.content]]

largeHtml :: DocumentTemplateFile
largeHtml = draftHtml {content = concat (replicate 4000 "<p class=\"note\">\196\\</p>\n")}

draftHtml :: DocumentTemplateFile
draftHtml = fileDefaultHtml {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "0b2d4f6a-8c1e-4a3b-9d5f-7e9a1c3b5d70"}

draftCss :: DocumentTemplateFile
draftCss = fileDefaultCss {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "1c3e5a7b-9d2f-4b4c-8e6a-0f2b4d6e8a91"}

draftSrc2Css :: DocumentTemplateFile
draftSrc2Css = fileDefaultCss {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "2d4f6b8c-0e3a-4c5d-9f7b-1a3c5e7f9b02", fileName = "src2/default.css"}

draftAsset :: DocumentTemplateAsset
draftAsset = assetLogo {documentTemplateUuid = wizardDocumentTemplateDraft.uuid, uuid = u' "3e5a7c9d-1f4b-4d6e-8a0c-2b4d6f8a0c13", fileName = "src/logo.txt"}

-- --------------------------------
-- HELPERS
-- --------------------------------
requestReadPages :: Value -> Maybe Value -> Int -> WaiSession st [SResponse]
requestReadPages arguments mCursor remaining = do
  response <- request methodPost "/api/mcp" [reqAuthHeader, reqCtHeader] (toolCallBody "read_document_template_files" (withCursor mCursor arguments))
  case valueField "cursor" =<< structuredContentOf response of
    Just cursor | remaining > 1 -> (response :) <$> requestReadPages arguments (Just cursor) (remaining - 1)
    _ -> return [response]

withCursor :: Maybe Value -> Value -> Value
withCursor (Just cursor) (Object o) = Object (KM.insert "cursor" cursor o)
withCursor _ arguments = arguments

truncatedOf :: SResponse -> Maybe Value
truncatedOf response = valueField "truncated" =<< structuredContentOf response

firstFileContent :: SResponse -> String
firstFileContent response =
  case valueField "files" =<< structuredContentOf response of
    Just (Array files) | Just (String content) <- valueField "thirdPartyContent" =<< valueField "content" =<< (files V.!? 0) -> T.unpack content
    _ -> ""

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertDocumentTemplateFilesInDB :: RequestContext -> U.UUID -> [(String, String)] -> WaiSession () ()
assertDocumentTemplateFilesInDB requestContext documentTemplateUuid expFiles = do
  files <- getOneFromDB (findFilesByDocumentTemplateUuid documentTemplateUuid) requestContext
  liftIO $ fmap (\file -> (file.fileName, file.content)) files `shouldMatchList` expFiles

assertDocumentTemplateFileNamesInDB :: RequestContext -> U.UUID -> [String] -> WaiSession () ()
assertDocumentTemplateFileNamesInDB requestContext documentTemplateUuid expFileNames = do
  files <- getOneFromDB (findFilesByDocumentTemplateUuid documentTemplateUuid) requestContext
  liftIO $ fmap (.fileName) files `shouldMatchList` expFileNames

assertDocumentTemplateAssetNamesInDB :: RequestContext -> U.UUID -> [String] -> WaiSession () ()
assertDocumentTemplateAssetNamesInDB requestContext documentTemplateUuid expFileNames = do
  assets <- getOneFromDB (findAssetsByDocumentTemplateUuid documentTemplateUuid) requestContext
  liftIO $ fmap (.fileName) assets `shouldMatchList` expFileNames

assertExistenceOfDocumentTemplateFileInDB :: RequestContext -> DocumentTemplateFile -> WaiSession () ()
assertExistenceOfDocumentTemplateFileInDB requestContext expFile = do
  file <- getOneFromDB (findFileByUuid expFile.uuid) requestContext
  liftIO $ file `shouldBe` expFile {tenantUuid = file.tenantUuid, createdAt = file.createdAt, updatedAt = file.updatedAt}

assertAbsenceOfDocumentTemplateAssetInS3 :: RequestContext -> DocumentTemplateAsset -> WaiSession () ()
assertAbsenceOfDocumentTemplateAssetInS3 requestContext asset = do
  eContent <- runInContextIO (retrieveAsset asset.documentTemplateUuid asset.uuid) requestContext
  liftIO $ isLeft eContent `shouldBe` True
