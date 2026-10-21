module Specs.Api.Handler.MailTemplateLocale.Registry.Detail_Pull_POST (
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
import Shared.Api.Resource.Locale.Registry.RegistryLocalePullJM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MTL_Migration
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.MailTemplateLocale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mail-template-locales/registry/{code}/pull
-- ------------------------------------------------------------------------
detail_pull_POST :: RequestContext -> SpecWith ((), Application)
detail_pull_POST requestContext =
  describe "POST /api/mail-template-locales/registry/{code}/pull" $ do
    test_200_preview requestContext
    test_200_replace requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mail-template-locales/registry/de/pull"

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
      let expDto = mailTemplateLocaleDeFromRegistryUploadPreviewDto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode registryLocalePreviewDto)
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      localeDeFromDb <- getOneFromDB (findMailTemplateLocaleByUuid mailTemplateLocaleDe.uuid) requestContext
      liftIO $ localeDeFromDb `shouldBe` mailTemplateLocaleDe

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_replace requestContext =
  it "HTTP 200 OK (a replace keeps the uuid, clears official and deletes mail.mo)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = mailTemplateLocaleDeFromRegistry
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      runMailTemplateLocaleMoMigration requestContext mailTemplateLocaleDe
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleUploadResultDTO MailTemplateLocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let (StoredLocaleUploadResultDTO replaced resLocale) = resDto
      liftIO $ replaced `shouldBe` True
      liftIO $ resLocale.uuid `shouldBe` expDto.uuid
      compareMailTemplateLocaleDtos resLocale expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfMailTemplateLocaleInDB requestContext expDto
      assertMailTemplateLocaleContentInS3 requestContext expDto deRegistryMailTemplateLocalePo
      assertAbsenceOfMailTemplateLocaleMoInS3 requestContext expDto

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
    "/api/mail-template-locales/registry/es/pull"
    reqHeaders
    reqBody
    "registry_mail_template_locale"
    [("code", "es")]
