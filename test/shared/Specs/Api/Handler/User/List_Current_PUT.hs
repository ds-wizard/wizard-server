module Specs.Api.Handler.User.List_Current_PUT (
  list_current_PUT,
) where

import Data.Aeson (encode)
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.User.User
import Shared.Service.User.WizardUserMapper
import WizardServer.Api.Resource.User.UserProfileChangeJM ()
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current
-- ------------------------------------------------------------------------
list_current_PUT :: RequestContext -> SpecWith ((), Application)
list_current_PUT requestContext =
  describe "PUT /wizard-api/users/current" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userAlbertEditedChange

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserDTO)
      let expDto = (toDTO userAlbertEditedAfterPut) {updatedAt = resDto.updatedAt} :: UserDTO
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareUserDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state (ignoring dynamic updatedAt)
      eUser <- runInContextIO (findUserByUuid userAlbert.uuid) requestContext
      liftIO $ isRight eUser `shouldBe` True
      let (Right userFromDB) = eUser
      let expUser = userAlbertEditedAfterPut {updatedAt = userFromDB.updatedAt} :: User
      liftIO $ userFromDB `shouldBe` expUser

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "password"
  it "HTTP 400 BAD REQUEST if email is already registered" $
    -- GIVEN: Prepare request
    do
      let reqDto = userIsaacProfileChange {email = userIsaac.email} :: UserProfileChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS reqDto.email])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
