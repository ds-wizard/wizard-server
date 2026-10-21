module Specs.Api.Handler.MailTemplateLocale.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Locale.LocaleChangeJM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
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
-- PUT /api/mail-template-locales/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/mail-template-locales/{uuid}" $ do
    test_200 requestContext
    test_400_disabled_default requestContext
    test_400_deactivate_default requestContext
    test_400_deactivate_and_unset_default requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/api/mail-template-locales/" ++ U.toString mailTemplateLocaleDe.uuid

reqCsUrl = BS.pack $ "/api/mail-template-locales/" ++ U.toString mailTemplateLocaleCs.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = localeDefaultChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (setting a default clears the previous one)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = mailTemplateLocaleDeDefault
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO (insertMailTemplateLocale mailTemplateLocaleCs) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, MailTemplateLocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.uuid `shouldBe` expDto.uuid
      compareMailTemplateLocaleDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfMailTemplateLocaleInDB requestContext expDto
      assertExistenceOfMailTemplateLocaleInDB requestContext mailTemplateLocaleCsNotDefault

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_disabled_default requestContext =
  it "HTTP 400 BAD REQUEST when a disabled locale is set as default" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO (insertMailTemplateLocale mailTemplateLocaleCs) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode localeDisabledDefaultChangeDto)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_deactivate_default requestContext =
  it "HTTP 400 BAD REQUEST when the default locale is disabled" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO (insertMailTemplateLocale mailTemplateLocaleCs) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqCsUrl reqHeaders (encode localeDisabledDefaultChangeDto)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_deactivate_and_unset_default requestContext =
  it "HTTP 400 BAD REQUEST when the default locale is disabled and unset" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO (insertMailTemplateLocale mailTemplateLocaleCs) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqCsUrl reqHeaders (encode localeDisabledChangeDto)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _SETTINGS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    reqUrl
    reqHeaders
    reqBody
    "mail_template_locale"
    [("uuid", U.toString mailTemplateLocaleDe.uuid)]
