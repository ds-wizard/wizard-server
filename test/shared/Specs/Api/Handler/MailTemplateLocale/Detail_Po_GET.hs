module Specs.Api.Handler.MailTemplateLocale.Detail_Po_GET (
  detail_po_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MTL_Migration
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/mail-template-locales/{uuid}/po
-- ------------------------------------------------------------------------
detail_po_GET :: RequestContext -> SpecWith ((), Application)
detail_po_GET requestContext =
  describe "GET /api/mail-template-locales/{uuid}/po" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/mail-template-locales/" ++ U.toString mailTemplateLocaleDe.uuid ++ "/po"

reqHeaders = [reqAuthHeader]

reqBody = ""

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
      runInContextIO MTL_Migration.runMigration requestContext
      runInContextIO MTL_Migration.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.contentType `shouldBe` "text/x-gettext-translation"
      liftIO $ ("de.po" `L.isInfixOf` resDto.url) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION

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
