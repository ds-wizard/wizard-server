module Specs.Service.Mcp.Tool.UserGroupTools.Common where

import Data.Aeson (Value (..))
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import Shared.Util.Date
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newGroupUuid :: U.UUID
newGroupUuid = u' "6e8a0c2d-4f6b-4d8f-ab2c-4e6a8c0e2f41"

newGroup :: UserGroup
newGroup = bioGroup {uuid = newGroupUuid, name = "Chemistry Group", description = Nothing, private = False, tenantUuid = defaultTenantUuid, workspaceUuid = defaultWorkspaceUuid}

newGroupMembership :: U.UUID -> UserGroupMembershipType -> UserGroupMembership
newGroupMembership userUuid mType = UserGroupMembership newGroupUuid userUuid mType defaultTenantUuid (dt' 2018 1 1) (dt' 2018 1 1)

groupNames :: Value -> [Value]
groupNames content =
  case valueField "userGroups" =<< valueField "_embedded" content of
    Just (Array groups) -> mapMaybe (valueField "name") (V.toList groups)
    _ -> []

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserGroupInDB :: RequestContext -> UserGroup -> WaiSession () ()
assertExistenceOfUserGroupInDB requestContext expUserGroup = do
  userGroup <- getOneFromDB (findUserGroupByUuid expUserGroup.uuid) requestContext
  liftIO $ userGroup `shouldBe` expUserGroup {createdAt = userGroup.createdAt, updatedAt = userGroup.updatedAt}

assertAbsenceOfUserGroupInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertAbsenceOfUserGroupInDB requestContext userGroupUuid = do
  mUserGroup <- getOneFromDB (findUserGroupByUuid' userGroupUuid) requestContext
  liftIO $ mUserGroup `shouldBe` Nothing

assertUserGroupMembershipsInDB :: RequestContext -> U.UUID -> [UserGroupMembership] -> WaiSession () ()
assertUserGroupMembershipsInDB requestContext userGroupUuid expMemberships = do
  memberships <- getOneFromDB (findUserGroupMembershipsByUserGroupUuid userGroupUuid) requestContext
  liftIO $ fmap withoutTimestamps memberships `shouldBe` fmap withoutTimestamps (L.sortOn (.userUuid) expMemberships)
  where
    withoutTimestamps membership = membership {createdAt = dt' 2018 1 1, updatedAt = dt' 2018 1 1} :: UserGroupMembership
