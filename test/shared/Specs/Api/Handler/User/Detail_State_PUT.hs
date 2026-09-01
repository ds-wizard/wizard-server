module Specs.Api.Handler.User.Detail_State_PUT (
  detail_state_PUT,
) where

import Data.Aeson (encode)
import Data.Time (getCurrentTime)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/{uuid}/state?hash={hash}
-- ------------------------------------------------------------------------
detail_state_PUT :: RequestContext -> SpecWith ((), Application)
detail_state_PUT requestContext =
  describe "PUT /wizard-api/users/{uuid}/state?hash={hash}" $ do
    test_200 requestContext
    test_400 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = userState

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCorsHeaders
      let expDto = reqDto
      let expBody = encode expDto
      -- AND: Prepare DB
      now <- liftIO getCurrentTime
      runInContextIO (insertUserEmailLink (registrationUserEmailLink {createdAt = now})) requestContext
      runInContextIO (updateUserByUuid (userAlbert {active = False})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink U.UUID UserEmailLinkType]) requestContext 0
      assertExistenceOfUserInDB requestContext userAlbert

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "active"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
