module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.WriteDocumentTemplateAssets (
  write_document_template_assets,
) where

import Data.Aeson
import Data.ByteArray.Encoding (Base (..), convertToBase)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Mcp
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (write_document_template_assets)
-- ------------------------------------------------------------------------
write_document_template_assets :: RequestContext -> SpecWith ((), Application)
write_document_template_assets requestContext =
  describe "POST /api/mcp (write_document_template_assets)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

imageContent :: BS.ByteString
imageContent = BS.pack "\137PNG\r\n\26\n"

base64 :: BS.ByteString -> String
base64 = BS.unpack . convertToBase Base64

reqArguments =
  draftArguments
    [ "assets"
        .= [ object ["path" .= ("src/image.png" :: String), "content" .= base64 imageContent, "content_type" .= ("image/png" :: String)]
           , object ["path" .= draftAsset.fileName, "new_path" .= ("src/notes.txt" :: String)]
           ]
    ]

reqBody = toolCallBody "write_document_template_assets" reqArguments

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
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "write_document_template_assets" structuredContent
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid ["src/image.png", "src/notes.txt"]
      [asset] <- filter ((== "src/image.png") . (.fileName)) <$> getOneFromDB (findAssetsByDocumentTemplateUuid wizardDocumentTemplateDraft.uuid) requestContext
      liftIO $ asset.contentType `shouldBe` "image/png"
      content <- getOneFromDB (retrieveAsset wizardDocumentTemplateDraft.uuid asset.uuid) requestContext
      liftIO $ content `shouldBe` imageContent
  it "HTTP 200 OK if an asset is replaced (content type kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [object ["path" .= draftAsset.fileName, "content" .= base64 (BS.pack "changed")]]])
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
      asset <- getOneFromDB (findAssetById draftAsset.uuid) requestContext
      liftIO $ asset.contentType `shouldBe` draftAsset.contentType
      content <- getOneFromDB (retrieveAsset wizardDocumentTemplateDraft.uuid draftAsset.uuid) requestContext
      liftIO $ content `shouldBe` BS.pack "changed"
  it "HTTP 200 OK if a new asset has no content_type (tool error, nothing of the batch stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody =
            toolCallBody
              "write_document_template_assets"
              (draftArguments ["assets" .= [object ["path" .= draftAsset.fileName, "new_path" .= ("src/notes.txt" :: String)], object ["path" .= ("a.png" :: String), "content" .= base64 imageContent]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__ASSET_NOT_FOUND "a.png")
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid [draftAsset.fileName]
  it "HTTP 200 OK if a later entry fails after a replacement (tool error, the replaced content unchanged)" $
    -- GIVEN: Prepare request
    do
      let replaceEntry = object ["path" .= draftAsset.fileName, "content" .= base64 (BS.pack "changed")]
      let failingEntry = object ["path" .= ("a.png" :: String), "content" .= base64 imageContent]
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [replaceEntry, failingEntry]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__ASSET_NOT_FOUND "a.png")
      -- AND: Run migrations
      runFileMigrations requestContext
      originalContent <- getOneFromDB (retrieveAsset wizardDocumentTemplateDraft.uuid draftAsset.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      content <- getOneFromDB (retrieveAsset wizardDocumentTemplateDraft.uuid draftAsset.uuid) requestContext
      liftIO $ content `shouldBe` originalContent
  it "HTTP 200 OK if an asset is renamed onto the path of another asset (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let createEntry = object ["path" .= ("src/other.png" :: String), "content" .= base64 imageContent, "content_type" .= ("image/png" :: String)]
      let renameEntry = object ["path" .= draftAsset.fileName, "new_path" .= ("src/other.png" :: String), "content" .= base64 imageContent]
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [createEntry, renameEntry]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS)
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid [draftAsset.fileName]
  it "HTTP 200 OK if the content is line-wrapped base64 with a data URL prefix" $
    -- GIVEN: Prepare request
    do
      let wrapped = "data:image/png;base64," ++ take 4 (base64 imageContent) ++ "\n" ++ drop 4 (base64 imageContent)
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [object ["path" .= ("a.png" :: String), "content" .= wrapped, "content_type" .= ("image/png" :: String)]]])
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
      [asset] <- filter ((== "a.png") . (.fileName)) <$> getOneFromDB (findAssetsByDocumentTemplateUuid wizardDocumentTemplateDraft.uuid) requestContext
      content <- getOneFromDB (retrieveAsset wizardDocumentTemplateDraft.uuid asset.uuid) requestContext
      liftIO $ content `shouldBe` imageContent
  it "HTTP 200 OK if the content is not base64 (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [object ["path" .= ("a.png" :: String), "content" .= ("not base64!" :: String), "content_type" .= ("image/png" :: String)]]])
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
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid [draftAsset.fileName]
  it "HTTP 200 OK if the batch is larger than the limit (tool error)" $
    -- GIVEN: Prepare request
    do
      let half = base64 (BS.replicate (mcpMaxBase64ContentSize `div` 2 + 1) 'x')
      let entry path = object ["path" .= (path :: String), "content" .= half, "content_type" .= ("text/plain" :: String)]
      let reqBody = toolCallBody "write_document_template_assets" (draftArguments ["assets" .= [entry "a.txt", entry "b.txt"]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MCP_BASE64_SIZE "assets" mcpMaxBase64ContentSize)
      -- AND: Run migrations
      runFileMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertDocumentTemplateAssetNamesInDB requestContext wizardDocumentTemplateDraft.uuid [draftAsset.fileName]
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
