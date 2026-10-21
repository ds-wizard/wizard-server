module Specs.Api.Handler.Locale.Registry.Detail_Pull_POST (
  detail_pull_POST,
) where

import Data.Aeson (encode)
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
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullJM ()
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Model.Locale.Locale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/locales/registry/{code}/pull
-- ------------------------------------------------------------------------
detail_pull_POST :: RequestContext -> SpecWith ((), Application)
detail_pull_POST requestContext =
  describe "POST /api/locales/registry/{code}/pull" $ do
    test_200_preview requestContext
    test_200_replace requestContext
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/locales/registry/de/pull"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = registryLocalePullDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_preview requestContext =
  it "HTTP 200 OK (preview of a replace)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = localeDeFromRegistryUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode registryLocalePreviewDto)
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
test_200_replace requestContext =
  it "HTTP 200 OK (a replace keeps the uuid and clears official)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeDeFromRegistry
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` True
      liftIO $ resLocale.uuid `shouldBe` expDto.uuid
      compareLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfLocaleInDB requestContext expDto
      assertLocaleContentInS3 requestContext expDto deRegistryLocalePo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (a new locale is stored disabled and not official)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = localeNlFromRegistry
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/locales/registry/nl/pull" reqHeaders reqBody
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
      assertLocaleContentInS3 requestContext (expDto {uuid = resLocale.uuid} :: Locale) nlRegistryLocalePo

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
  createNotFoundTest
    reqMethod
    "/api/locales/registry/es/pull"
    reqHeaders
    reqBody
    "registry_locale"
    [("code", "es")]
