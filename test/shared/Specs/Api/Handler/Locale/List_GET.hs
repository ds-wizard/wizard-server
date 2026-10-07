module Specs.Api.Handler.Locale.List_GET (
  list_GET,
) where

import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Model.User.RolePermission
import Shared.Service.Locale.LocaleMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/locales" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/locales"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (only the locales of the tenant)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = fmap toDTO [localeCs, localeDe]
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, [LocaleDTO])
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ L.sortOn (.code) resDto `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody _ORGANIZATION_SETTINGS_MANAGE_ROLE_PERMISSION
