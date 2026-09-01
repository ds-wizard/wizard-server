module Specs.Api.Handler.User.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.UserEmailLink.UserEmailLinkMigration as ACK
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/users
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/users" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/users"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT dto = dto

reqBodyT dto = encode (reqDtoT dto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED (anonymous)" requestContext userJohnCreate userJohnCreateDS [] 1 False
  create_test_201 "HTTP 201 CREATED (admin)" requestContext userJohnCreate userJohnCreate [reqAuthHeader] 0 True

create_test_201 title requestContext reqDto expDto authHeaders persistentCommandCount userActive =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqBody = reqBodyT reqDto
      -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO ACK.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareUserCreateDtos resDto expDto userActive
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink U.UUID UserEmailLinkType]) requestContext 1
      assertCountInDB findUsers requestContext 2
      assertCountInDB (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext persistentCommandCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  create_test_400_email_uniqueness
    "HTTP 400 BAD REQUEST if email is already registered (admin)"
    requestContext
    [reqAuthHeader]

create_test_400_email_uniqueness title requestContext authHeaders =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqDto = userJohnCreate {email = userAlbert.email} :: UserCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS reqDto.email])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
