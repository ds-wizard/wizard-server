module Specs.Api.Handler.User.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.User.UserDetailJM ()
import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OpenIdClient
import Shared.Database.Migration.Development.User.Data.UserOpenIdIdentities
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.User.User
import Shared.Service.User.UserOpenIdIdentityMapper (fromCreate)
import Shared.Service.User.WizardUserMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/users/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/users/{uuid}" $ do
    test_200 requestContext
    test_200_with_identity requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"

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
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toUserDetailDTO userAlbert []
      let expBody = encode expDto
      -- AND: Prepare database
      runInContextIO U.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_with_identity requestContext =
  it "HTTP 200 OK (with external identity)" $
    -- GIVEN: Prepare expectation
    do
      let identityDto = defaultUserOpenIdIdentityDto
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toUserDetailDTO userAlbert [identityDto]
      let expBody = encode expDto
      -- AND: Prepare database
      runInContextIO U.runMigration requestContext
      runInContextIO OpenIdClient.runMigration requestContext
      runInContextIO
        ( insertUserOpenIdIdentity
            (fromCreate identityDto.uuid identityDto.externalId identityDto.externalLabel userAlbert.uuid identityDto.providerUuid userAlbert.tenantUuid identityDto.createdAt)
        )
        requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] "" "users.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "user_entity"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
