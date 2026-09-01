module Specs.Api.Handler.DocumentTemplate.Detail_Locales_POST (
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

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/document-templates/{uuid}/locales
-- ------------------------------------------------------------------------
detail_locales_POST :: RequestContext -> SpecWith ((), Application)
detail_locales_POST requestContext =
  describe "POST /wizard-api/document-templates/{uuid}/locales" $ do
    test_200 requestContext
    test_400_missing_language requestContext
    test_400_duplicate_code requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/wizard-api/document-templates/" ++ show wizardDocumentTemplate.uuid ++ "/locales"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody "German" germanPoContent

createMultipartBody :: String -> BS.ByteString -> BSL.ByteString
createMultipartBody name poContent =
  BSL.fromStrict . BS.concat $
    [ BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"name\"\r\n\r\n"
    , BS.pack $ name ++ "\r\n"
    , BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"poContent\"; filename=\"translation.po\"\r\n"
    , BS.pack "Content-Type: application/octet-stream\r\n\r\n"
    , poContent
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
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateLocaleList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.name `shouldBe` "German"
      liftIO $ resBody.code `shouldBe` "de"
      -- AND: Find result in DB and compare with expectation state
      eLocaleFromDb <- runInContextIO (findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' wizardDocumentTemplate.uuid "de") requestContext
      liftIO $ fmap isJust eLocaleFromDb `shouldBe` Right True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_missing_language requestContext =
  it "HTTP 400 BAD REQUEST when PO file has no Language header" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__DOC_TML_LOCALE_MISSING_LANGUAGE
    let expBody = encode expDto
    runInContextIO DT_Migration.runMigration requestContext
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody "German" poContentWithoutLanguage)
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
    let expDto = UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_CODE_UNIQUENESS "cs"
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO DT_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody "Czech" czechPoContent)
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/document-templates/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales"
    reqHeaders
    reqBody
    "document_template"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
