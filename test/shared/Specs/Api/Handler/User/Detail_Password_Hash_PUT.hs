module Specs.Api.Handler.User.Detail_Password_Hash_PUT (
  detail_password_hash_PUT,
) where

import Data.Aeson (encode)
import Data.Time (getCurrentTime)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/{uuid}/password?hash={hash}
-- ------------------------------------------------------------------------
detail_password_hash_PUT :: RequestContext -> SpecWith ((), Application)
detail_password_hash_PUT requestContext =
  describe "PUT /wizard-api/users/{uuid}/password?hash={hash}" $ do
    test_204 requestContext
    test_400 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = userPassword

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare DB
    do
      now <- liftIO getCurrentTime
      eitherUserEmailLink <- runInContextIO (insertUserEmailLink (forgottenPasswordUserEmailLink {createdAt = now})) requestContext
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertPasswordOfUserInDB requestContext userAlbert userPassword.password

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
