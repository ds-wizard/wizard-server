module Specs.Api.Handler.User.List_Current_Password_PUT (
  list_current_password_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserDTO ()
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.User
import WizardServer.Api.Resource.User.UserProfileChangeJM ()
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/password
-- ------------------------------------------------------------------------
list_current_password_PUT :: RequestContext -> SpecWith ((), Application)
list_current_password_PUT requestContext =
  describe "PUT /wizard-api/users/current/password" $ do
    test_204 requestContext
    test_400_invalid_json requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current/password"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userPassword

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertPasswordOfUserInDB requestContext userAlbert userPassword.password

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
