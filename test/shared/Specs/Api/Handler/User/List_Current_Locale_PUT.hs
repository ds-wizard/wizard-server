module Specs.Api.Handler.User.List_Current_Locale_PUT (
  list_current_locale_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Database.Migration.Development.User.Data.WizardUsers
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/locale
-- ------------------------------------------------------------------------
list_current_locale_PUT :: RequestContext -> SpecWith ((), Application)
list_current_locale_PUT requestContext =
  describe "PUT /wizard-api/users/current/locale" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current/locale"

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
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
