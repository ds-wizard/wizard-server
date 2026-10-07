module Specs.Api.Handler.User.List_Current_Locale_PUT (
  list_current_locale_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/users/current/locale
-- ------------------------------------------------------------------------
list_current_locale_PUT :: RequestContext -> SpecWith ((), Application)
list_current_locale_PUT requestContext =
  describe "PUT /api/users/current/locale" $ do
    test_200 requestContext
    test_400_disabled_locale requestContext
    test_400_different_tenant_locale requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/users/current/locale"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userLocale

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = userLocale
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserInDB requestContext userAlbertEditedLocale

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_disabled_locale requestContext =
  it "HTTP 400 BAD REQUEST when the locale is disabled" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString localeCs.uuid)])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode userLocaleDisabled)
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_different_tenant_locale requestContext =
  it "HTTP 400 BAD REQUEST when the locale belongs to another tenant" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString differentLocale.uuid)])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (encode userLocaleDifferentTenant)
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
