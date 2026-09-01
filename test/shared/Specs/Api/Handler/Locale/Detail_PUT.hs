module Specs.Api.Handler.Locale.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Model.Locale.Locale
import WizardServer.Api.Resource.Locale.LocaleChangeJM ()
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleJM ()
import WizardServer.Database.Migration.Development.Locale.Data.Locales
import WizardServer.Model.Context.RequestContext
import WizardServer.Service.Locale.LocaleMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Locale.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/locales/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/locales/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/locales/d9894fb9-c6a5-4294-98d6-b46d75684d53"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = localeNlChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = toDTO False $ toLocaleList localeNlEdited
    -- AND: Run migrations
    runInContextIO LOC_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareLocaleDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfLocaleInDB requestContext localeNlEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "SettingsManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/locales/99193032-99e3-4676-acd8-222983ea0b88"
    reqHeaders
    reqBody
    "locale"
    [("uuid", "99193032-99e3-4676-acd8-222983ea0b88")]
