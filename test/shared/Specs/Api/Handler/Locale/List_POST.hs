module Specs.Api.Handler.Locale.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultJM ()
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/locales
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/locales" $ do
    test_200_preview requestContext
    test_200_preview_replace requestContext
    test_200_preview_without_pot_version requestContext
    test_200 requestContext
    test_200_replace requestContext
    test_400_missing_language requestContext
    test_400_unparsable requestContext
    test_400_mail_template_kind requestContext
    test_400_invalid_language requestContext
    test_400_too_large requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/locales"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody localeSvPo True

createMultipartBody :: BS.ByteString -> Bool -> BSL.ByteString
createMultipartBody poContent confirmed =
  BSL.fromStrict . BS.concat $
    [ BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"file\"; filename=\"locale.po\"\r\n"
    , BS.pack "Content-Type: text/x-gettext-translation\r\n\r\n"
    , poContent
    , BS.pack "\r\n"
    ]
      ++ (if confirmed then confirmedPart else [])
      ++ [BS.pack $ "--" ++ boundary ++ "--\r\n"]

confirmedPart =
  [ BS.pack $ "--" ++ boundary ++ "\r\n"
  , BS.pack "Content-Disposition: form-data; name=\"confirmed\"\r\n\r\n"
  , BS.pack "true\r\n"
  ]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_preview requestContext =
  it "HTTP 200 OK (preview of a new locale, code normalized)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = localeSvUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localeSvPo False)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 2
      localeDeFromDb <- getOneFromDB (findLocaleByUuid localeDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` localeDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_preview_replace requestContext =
  it "HTTP 200 OK (preview of a replace of an official locale)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = localeDeEditedUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localeDeEditedPo False)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 2
      localeDeFromDb <- getOneFromDB (findLocaleByUuid localeDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` localeDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_preview_without_pot_version requestContext =
  it "HTTP 200 OK (preview without Project-Id-Version)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = localeFrUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localeFrPoWithoutPotVersion False)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 2
      localeDeFromDb <- getOneFromDB (findLocaleByUuid localeDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` localeDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (a new locale is stored disabled)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeSv
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` False
      compareLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 3
      assertExistenceOfLocaleInDB requestContext (expDto {uuid = resLocale.uuid} :: Locale)
      assertLocaleContentInS3 requestContext (expDto {uuid = resLocale.uuid} :: Locale) localeSvPo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_replace requestContext =
  it "HTTP 200 OK (a replace keeps uuid, enabled and default, and clears official)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeDeEdited
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localeDeEditedPo True)
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` True
      liftIO $ resLocale.uuid `shouldBe` expDto.uuid
      compareLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 2
      assertExistenceOfLocaleInDB requestContext expDto
      localeFromDb <- getOneFromDB (findLocaleByUuid expDto.uuid) requestContext
      liftIO $ (localeFromDb.updatedAt > localeDe.updatedAt) `shouldBe` True
      assertLocaleContentInS3 requestContext expDto localeDeEditedPo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_missing_language requestContext =
  it "HTTP 400 BAD REQUEST when PO file has no Language header" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__LOCALE_PO_MISSING_LANGUAGE
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localePoWithoutLanguage True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_unparsable requestContext =
  it "HTTP 400 BAD REQUEST when PO file is unparsable" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__LOCALE_PO_UNPARSABLE "Unexpected keyword msgstr"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localePoUnparsable True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_mail_template_kind requestContext =
  it "HTTP 400 BAD REQUEST when PO file is a mail template translation" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__LOCALE_PO_UNSUPPORTED_KIND "fw-mail-templates"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localePoMailTemplateKind True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_language requestContext =
  it "HTTP 400 BAD REQUEST when PO file has an invalid Language header" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__LOCALE_PO_INVALID_LANGUAGE "deutsch"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localePoInvalidLanguage True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_too_large requestContext =
  it "HTTP 400 BAD REQUEST when PO file is larger than 5 MB" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__LOCALE_PO_TOO_LARGE
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody localePoTooLarge True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _SETTINGS_MANAGE_ROLE_PERMISSION
