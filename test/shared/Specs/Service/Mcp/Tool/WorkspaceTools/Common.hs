module Specs.Service.Mcp.Tool.WorkspaceTools.Common where

import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newWorkspaceUuid :: U.UUID
newWorkspaceUuid = u' "5d7f9b1c-3e5a-4c7e-9a1b-3d5f7a9c1e30"

newWorkspace :: Workspace
newWorkspace = defaultWorkspace {uuid = newWorkspaceUuid, name = "Chemistry", description = Just "Chemistry projects"}

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfWorkspaceMembershipInDB :: RequestContext -> U.UUID -> U.UUID -> Maybe U.UUID -> WaiSession () ()
assertExistenceOfWorkspaceMembershipInDB requestContext workspaceUuid userUuid mRoleUuid = do
  membership <- getOneFromDB (findWorkspaceMembership workspaceUuid userUuid) requestContext
  let expRoleUuid = fromMaybe membership.roleUuid mRoleUuid
  liftIO $ membership `shouldBe` WorkspaceMembership workspaceUuid userUuid defaultTenantUuid membership.createdAt expRoleUuid

assertAbsenceOfWorkspaceMembershipInDB :: RequestContext -> U.UUID -> U.UUID -> WaiSession () ()
assertAbsenceOfWorkspaceMembershipInDB requestContext workspaceUuid userUuid = do
  mMembership <- getOneFromDB (findWorkspaceMembership' workspaceUuid userUuid) requestContext
  liftIO $ mMembership `shouldBe` Nothing

assertWorkspaceInDB :: RequestContext -> U.UUID -> Bool -> WaiSession () ()
assertWorkspaceInDB requestContext uuid expExists = do
  mWorkspace <- getOneFromDB (findWorkspaceByUuid' uuid) requestContext
  liftIO $ fmap (.uuid) mWorkspace `shouldBe` (if expExists then Just uuid else Nothing)
