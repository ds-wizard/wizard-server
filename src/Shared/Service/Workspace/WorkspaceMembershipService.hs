module Shared.Service.Workspace.WorkspaceMembershipService where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Service.Workspace.WorkspaceMapper

addUserToSingleWorkspace :: WizardRequestContextC s m => U.UUID -> U.UUID -> UTCTime -> m ()
addUserToSingleWorkspace tenantUuid userUuid now = do
  tenant <- findTenantByUuid tenantUuid
  unless tenant.multiWorkspace $ do
    workspaceUuids <- findWorkspaceUuidsByTenantUuid tenantUuid
    case workspaceUuids of
      [workspaceUuid] -> addWorkspaceMembership workspaceUuid userUuid tenantUuid now
      [] -> throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
      _ -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__MULTIPLE_WORKSPACES

addWorkspaceMembership :: WizardRequestContextC s m => U.UUID -> U.UUID -> U.UUID -> UTCTime -> m ()
addWorkspaceMembership workspaceUuid userUuid tenantUuid now =
  void $ insertWorkspaceMembership (toMembership workspaceUuid userUuid tenantUuid now)

removeWorkspaceMembership :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeWorkspaceMembership workspaceUuid userUuid = do
  deleteUserGroupMembershipsInWorkspaceByUserUuid workspaceUuid userUuid
  deleteProjectPermsInWorkspaceByUserUuid workspaceUuid userUuid
  void $ deleteWorkspaceMembership workspaceUuid userUuid
