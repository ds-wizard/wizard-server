module Specs.Api.Handler.MailTemplateLocale.List_POST (
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
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MTL_Migration
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Error.Error
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mail-template-locales
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/mail-template-locales" $ do
    test_200_preview requestContext
    test_200_preview_replace requestContext
    test_200 requestContext
    test_200_replace requestContext
    test_400_missing_language requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mail-template-locales"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody mailTemplateLocaleNlPo True

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
  it "HTTP 200 OK (preview of a new locale with any kind)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplateLocaleNlCustomKindUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody mailTemplateLocaleNlCustomKindPo False)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMailTemplateLocales requestContext 1
      localeDeFromDb <- getOneFromDB (findMailTemplateLocaleByUuid mailTemplateLocaleDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` mailTemplateLocaleDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_preview_replace requestContext =
  it "HTTP 200 OK (preview of a replace of an official locale)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = mailTemplateLocaleDeEditedUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody mailTemplateLocaleDeEditedPo False)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMailTemplateLocales requestContext 1
      localeDeFromDb <- getOneFromDB (findMailTemplateLocaleByUuid mailTemplateLocaleDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` mailTemplateLocaleDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (a new locale is stored disabled, a UI kind is not rejected)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = mailTemplateLocaleNl
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO MailTemplateLocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` False
      compareMailTemplateLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfMailTemplateLocaleInDB requestContext (expDto {uuid = resLocale.uuid} :: MailTemplateLocale)
      assertMailTemplateLocaleContentInS3 requestContext (expDto {uuid = resLocale.uuid} :: MailTemplateLocale) mailTemplateLocaleNlPo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_replace requestContext =
  it "HTTP 200 OK (a replace keeps uuid, enabled and default, clears official and deletes mail.mo)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = mailTemplateLocaleDeEdited
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO (updateMailTemplateLocaleByUuid mailTemplateLocaleDeDefault) requestContext
      runInContextIO MTL_Migration.runS3Migration requestContext
      runMailTemplateLocaleMoMigration requestContext mailTemplateLocaleDe
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody mailTemplateLocaleDeEditedPo True)
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO MailTemplateLocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` True
      liftIO $ resLocale.uuid `shouldBe` expDto.uuid
      compareMailTemplateLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMailTemplateLocales requestContext 1
      assertExistenceOfMailTemplateLocaleInDB requestContext expDto
      localeFromDb <- getOneFromDB (findMailTemplateLocaleByUuid expDto.uuid) requestContext
      liftIO $ (localeFromDb.updatedAt > mailTemplateLocaleDe.updatedAt) `shouldBe` True
      assertMailTemplateLocaleContentInS3 requestContext expDto mailTemplateLocaleDeEditedPo
      assertAbsenceOfMailTemplateLocaleMoInS3 requestContext expDto

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
      response <- request reqMethod reqUrl reqHeaders (createMultipartBody mailTemplateLocalePoWithoutLanguage True)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMailTemplateLocales requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _SETTINGS_MANAGE_ROLE_PERMISSION
