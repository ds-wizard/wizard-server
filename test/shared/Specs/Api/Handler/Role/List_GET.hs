module Specs.Api.Handler.Role.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import qualified Shared.Service.User.RoleMapper as Mapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/roles
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/roles" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/roles"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (sort by uuid asc)"
    requestContext
    "/wizard-api/roles?sort=uuid,asc"
    ( Page
        "roles"
        (PageMetadata 20 3 1 0)
        [Mapper.toDTO adminRole 2, Mapper.toDTO dataStewardRole 1, Mapper.toDTO researcherRole 1]
    )
  create_test_200
    "HTTP 200 OK (pagination)"
    requestContext
    "/wizard-api/roles?sort=uuid,asc&page=1&size=1"
    (Page "roles" (PageMetadata 1 3 3 1) [Mapper.toDTO dataStewardRole 1])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/wizard-api/roles?sort=uuid,asc&q=Researcher"
    (Page "roles" (PageMetadata 20 1 1 0) [Mapper.toDTO researcherRole 1])

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
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
test_403 requestContext = createNoPermissionsAnyTest requestContext reqMethod reqUrl [] reqBody ["SettingsManageRolePermission", "UsersManageRolePermission"]
