module Specs.Api.Handler.UserGroup.Membership.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeJM ()
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/user-groups/{uuid}/memberships
-- ------------------------------------------------------------------------
list_PUT :: RequestContext -> SpecWith ((), Application)
list_PUT requestContext =
  describe "PUT /api/user-groups/{uuid}/memberships" $ do
    test_200 requestContext
    test_200_type_change requestContext
    test_400 requestContext
    test_400_duplicate requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/user-groups/6ab8ecc3-c0f5-4864-a055-b0096ca55569/memberships"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto =
  [ UserGroupMembershipChangeDTO {userUuid = userAlbert.uuid, mType = OwnerUserGroupMembershipType}
  , UserGroupMembershipChangeDTO {userUuid = userIsaac.uuid, mType = MemberUserGroupMembershipType}
  ]

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK (adds Isaac, drops Nikola)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 reqDto response
    -- AND: Find result in DB and compare with expectation state
    memberships <- getOneFromDB (findUserGroupMembershipsByUserGroupUuid bioGroup.uuid) requestContext
    liftIO $ fmap (\m -> (m.userUuid, m.mType)) memberships `shouldMatchList` fmap (\m -> (m.userUuid, m.mType)) reqDto

test_200_type_change requestContext =
  it "HTTP 200 OK (changes the type of an existing member)" $ do
    -- GIVEN: Prepare expectation
    let dto =
          [ UserGroupMembershipChangeDTO {userUuid = userAlbert.uuid, mType = OwnerUserGroupMembershipType}
          , UserGroupMembershipChangeDTO {userUuid = userNikola.uuid, mType = MemberUserGroupMembershipType}
          ]
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders (encode dto)
    -- THEN: Compare response with expectation
    expectJson 200 dto response
    -- AND: Find result in DB and compare with expectation state
    memberships <- getOneFromDB (findUserGroupMembershipsByUserGroupUuid bioGroup.uuid) requestContext
    liftIO $ fmap (\m -> (m.userUuid, m.mType)) memberships `shouldMatchList` fmap (\m -> (m.userUuid, m.mType)) dto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (user is not a member of the workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    let body = encode [UserGroupMembershipChangeDTO {userUuid = userCharles.uuid, mType = MemberUserGroupMembershipType}]
    response <- request reqMethod reqUrl reqHeaders body
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__NOT_MEMBER) response

test_400_duplicate requestContext =
  it "HTTP 400 BAD REQUEST (a user listed twice)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    let body =
          encode
            [ UserGroupMembershipChangeDTO {userUuid = userAlbert.uuid, mType = OwnerUserGroupMembershipType}
            , UserGroupMembershipChangeDTO {userUuid = userAlbert.uuid, mType = MemberUserGroupMembershipType}
            ]
    response <- request reqMethod reqUrl reqHeaders body
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_USER_GROUP__DUPLICATE_MEMBER) response

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
