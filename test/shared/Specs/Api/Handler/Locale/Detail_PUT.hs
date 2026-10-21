module Specs.Api.Handler.Locale.Detail_PUT (
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
-- PUT /api/locales/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/locales/{uuid}" $ do
    test_200 requestContext
    test_200_default requestContext
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

reqUrl = BS.pack $ "/api/locales/" ++ U.toString localeCs.uuid

reqDeUrl = BS.pack $ "/api/locales/" ++ U.toString localeDe.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = localeEnabledChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (enable a locale)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeCsEnabled
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.uuid `shouldBe` expDto.uuid
      compareLocaleDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfLocaleInDB requestContext expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_default requestContext =
  it "HTTP 200 OK (setting a default clears the previous one)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeCsDefault
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode localeDefaultChangeDto)
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.uuid `shouldBe` expDto.uuid
      compareLocaleDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfLocaleInDB requestContext expDto
      assertExistenceOfLocaleInDB requestContext localeDe

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
      runInContextIO LOC_Migration.runMigration requestContext
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
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqDeUrl reqHeaders (encode localeDisabledDefaultChangeDto)
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
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO (updateLocaleByUuid localeDeDefault) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqDeUrl reqHeaders (encode localeDisabledChangeDto)
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
    (BS.pack $ "/api/locales/" ++ U.toString differentLocale.uuid)
    reqHeaders
    reqBody
    "locale"
    [("uuid", U.toString differentLocale.uuid)]
