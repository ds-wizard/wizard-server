module Specs.Api.Handler.UserGroup.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Api.Resource.User.Group.UserGroupJM ()
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.Migration.Development.User.Data.UserGroups
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.User.UserGroup
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/user-groups/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/user-groups/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/user-groups/6ab8ecc3-c0f5-4864-a055-b0096ca55569"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = encode userGroupChange

expUserGroup = bioGroup {name = userGroupChange.name, description = userGroupChange.description, private = userGroupChange.private} :: UserGroup

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, UserGroup)
    assertResStatus status 200
    liftIO $ compareUserGroups resBody expUserGroup `shouldBe` True
    -- AND: Find result in DB and compare with expectation state
    userGroupFromDb <- getOneFromDB (findUserGroupByUuid bioGroup.uuid) requestContext
    liftIO $ compareUserGroups userGroupFromDb expUserGroup `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (neither owner nor userGroups.manage)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqIsaacAuthTokenHeader, reqCtHeader] reqBody
    -- THEN: Compare response with expectation
    expectJson 403 (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit UserGroup") response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/user-groups/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "user_group"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
