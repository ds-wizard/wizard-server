module Specs.Api.Handler.Locale.Detail_Po_GET (
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
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Model.Locale.Locale
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales/{uuid}/po
-- ------------------------------------------------------------------------
detail_po_GET :: RequestContext -> SpecWith ((), Application)
detail_po_GET requestContext =
  describe "GET /api/locales/{uuid}/po" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/locales/" ++ U.toString localeDe.uuid ++ "/po"

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
      runInContextIO LOC_Migration.runMigration requestContext
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
    (BS.pack $ "/api/locales/" ++ U.toString differentLocale.uuid ++ "/po")
    reqHeaders
    reqBody
    "locale"
    [("uuid", U.toString differentLocale.uuid)]
