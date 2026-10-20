module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.ReadDocumentTemplateAssets (
  read_document_template_assets,
) where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import Shared.Util.Uuid
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (read_document_template_assets)
-- ------------------------------------------------------------------------
read_document_template_assets :: RequestContext -> SpecWith ((), Application)
read_document_template_assets requestContext =
  describe "POST /api/mcp (read_document_template_assets)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = draftArguments ["paths" .= [draftAsset.fileName, imageAsset.fileName, "missing.png"]]

reqBody = toolCallBody "read_document_template_assets" reqArguments

imageAsset :: DocumentTemplateAsset
imageAsset = draftAsset {uuid = u' "4f6b8d0e-2a5c-4e7f-9b1d-3c5e7a9b1d24", fileName = "src/logo.png", contentType = "image/png"}

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
      runInContextIO (void $ insertAsset imageAsset) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Just (Array assets) = valueField "assets" structuredContent
      let [textAsset, binaryAsset] = V.toList assets
      liftIO $ stringField "fileName" textAsset `shouldBe` Just draftAsset.fileName
      liftIO $ (stringField "thirdPartyContent" =<< valueField "content" textAsset) `shouldBe` Just (BS.unpack assetLogoContent)
      liftIO $ stringField "fileName" binaryAsset `shouldBe` Just imageAsset.fileName
      liftIO $ valueField "content" binaryAsset `shouldBe` Nothing
      liftIO $ stringField "url" binaryAsset `shouldSatisfy` (/= Nothing)
      liftIO $ valueField "missing" structuredContent `shouldBe` Just (toJSON ["missing.png" :: String])
      assertToolOutputSchema reqHeaders "read_document_template_assets" structuredContent
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
