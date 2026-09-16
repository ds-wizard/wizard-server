module Specs.Api.Handler.Role.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.User.RoleDAO (insertRole)
import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.User.Role
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Role.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/roles/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/roles/{uuid}" $ do
    test_204 requestContext
    test_400_admin requestContext
    test_400_default requestContext
    test_400_in_use requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/roles/1df6b458-304f-440b-b9e6-2f87e75f2eee"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO (insertRole deletableRole) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfRoleInDB requestContext deletableRole.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_admin requestContext = createDeleteValidationTest requestContext "the admin role is deleted" "378bdd91-96e4-44ad-9b39-25cd3cdde18d" _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_DELETED

test_400_default requestContext = createDeleteValidationTest requestContext "the default role is deleted" "4e79c242-4100-4447-becd-45eca47fa60b" _ERROR_VALIDATION__USER_ROLE_IS_DEFAULT

test_400_in_use requestContext = createDeleteValidationTest requestContext "the role is assigned to users" "65f88e18-8a24-4f70-92e7-8f20f7559600" _ERROR_VALIDATION__USER_ROLE_IN_USE

createDeleteValidationTest requestContext title roleUuid expError =
  it ("HTTP 400 BAD REQUEST when " ++ title) $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError expError
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod ("/api/roles/" `mappend` roleUuid) reqHeaders reqBody
    -- THEN: Compare response with expectation
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
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "SettingsManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/roles/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "role"
    [("tenant_uuid", "00000000-0000-0000-0000-000000000000"), ("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
