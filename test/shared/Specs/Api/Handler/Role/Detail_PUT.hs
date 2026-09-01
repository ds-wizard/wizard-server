module Specs.Api.Handler.Role.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.RoleChangeJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Role.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/roles/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /wizard-api/roles/{uuid}" $ do
    test_200 requestContext
    test_400_invalid requestContext
    test_400_admin requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/roles/a0000000-0000-0000-0000-000000000002"

reqUrlAdmin = "/wizard-api/roles/a0000000-0000-0000-0000-000000000001"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: RoleChangeDTO
reqDto =
  RoleChangeDTO
    { name = "Data Steward (edited)"
    , permissions = [_KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          RoleList
            { uuid = dataStewardRole.uuid
            , name = reqDto.name
            , permissions = reqDto.permissions
            , usersCount = 1
            , isAdmin = False
            }
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, RoleList)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareRoleDtos resDto expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid requestContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_admin requestContext =
  it "HTTP 400 BAD REQUEST when the admin role is changed" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlAdmin reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

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
  createNotFoundTest
    reqMethod
    "/wizard-api/roles/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "role"
    [("tenant_uuid", "00000000-0000-0000-0000-000000000000"), ("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
