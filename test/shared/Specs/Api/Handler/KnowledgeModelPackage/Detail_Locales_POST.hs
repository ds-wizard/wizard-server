module Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_POST (
  detail_locales_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-packages/{uuid}/locales
-- ------------------------------------------------------------------------
detail_locales_POST :: RequestContext -> SpecWith ((), Application)
detail_locales_POST requestContext =
  describe "POST /wizard-api/knowledge-model-packages/{uuid}/locales" $ do
    test_200 requestContext
    test_400_missing_language requestContext
    test_400_duplicate_code requestContext
    test_400_invalid_json_content requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody czechPoContent czechJsonContent

createMultipartBody :: BS.ByteString -> BS.ByteString -> BSL.ByteString
createMultipartBody poContent jsonContent =
  BSL.fromStrict . BS.concat $
    [ BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"name\"\r\n\r\n"
    , BS.pack "Czech\r\n"
    , BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"poContent\"; filename=\"translation.po\"\r\n"
    , BS.pack "Content-Type: application/octet-stream\r\n\r\n"
    , poContent
    , BS.pack "\r\n"
    , BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"jsonContent\"; filename=\"translation.json\"\r\n"
    , BS.pack "Content-Type: application/json\r\n\r\n"
    , jsonContent
    , BS.pack "\r\n"
    , BS.pack $ "--" ++ boundary ++ "--\r\n"
    ]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelLocaleList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.name `shouldBe` "Czech"
      liftIO $ resBody.code `shouldBe` "cs"
      -- AND: Find result in DB and compare with expectation state
      eLocaleFromDb <- runInContextIO (findKnowledgeModelLocaleByPackageUuidAndCode' globalKmPackage.uuid "cs") requestContext
      liftIO $ fmap isJust eLocaleFromDb `shouldBe` Right True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_missing_language requestContext =
  it "HTTP 400 BAD REQUEST when PO file has no Language header" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE
    let expBody = encode expDto
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody poContentWithoutLanguage czechJsonContent)
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_duplicate_code requestContext =
  it "HTTP 400 BAD REQUEST when locale for the language already exists" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError $ _ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS "cs"
    let expBody = encode expDto
    -- AND: Seed locale with the same code
    runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json_content requestContext =
  it "HTTP 400 BAD REQUEST when JSON translation file is invalid" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody czechPoContent (BS.pack "{invalid"))
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
