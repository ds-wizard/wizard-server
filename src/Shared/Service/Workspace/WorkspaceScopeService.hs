module Shared.Service.Workspace.WorkspaceScopeService where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Model.Workspace.Workspace

resolveScope :: WizardRequestContextC s m => Maybe U.UUID -> Maybe Bool -> m Scope
resolveScope (Just _) (Just True) = throwError $ UserError _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT
resolveScope _ (Just True) = return TenantScope
resolveScope (Just workspaceUuid) _ = return $ WorkspaceScope workspaceUuid
resolveScope _ _ = return NoScope

requireWorkspaceScope :: WizardRequestContextC s m => m U.UUID
requireWorkspaceScope = do
  scope <- asks (.scope')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  case scope of
    TenantScope -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED
    WorkspaceScope workspaceUuid -> do
      unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
      checkWorkspaceMembership workspaceUuid
      return workspaceUuid
    NoScope ->
      if multiWorkspace
        then throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED
        else getOnlyWorkspaceUuid

requirePlaneScope :: WizardRequestContextC s m => m (Maybe U.UUID)
requirePlaneScope = do
  scope <- asks (.scope')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  case scope of
    TenantScope -> return Nothing
    WorkspaceScope workspaceUuid -> do
      unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
      checkWorkspaceMembership workspaceUuid
      return $ Just workspaceUuid
    NoScope ->
      if multiWorkspace
        then throwError $ UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED
        else return Nothing

resolveCommandWorkspaceUuid :: WizardRequestContextC s m => Maybe U.UUID -> m U.UUID
resolveCommandWorkspaceUuid (Just workspaceUuid) = (.uuid) <$> findWorkspaceByUuid workspaceUuid
resolveCommandWorkspaceUuid Nothing = do
  tenantUuid <- asks (.tenantUuid')
  tenant <- findTenantByUuid tenantUuid
  when tenant.multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED)
  getOnlyWorkspaceUuid

getOnlyWorkspaceUuid :: WizardRequestContextC s m => m U.UUID
getOnlyWorkspaceUuid = do
  workspaceUuids <- findWorkspaceUuids
  case workspaceUuids of
    [workspaceUuid] -> return workspaceUuid
    [] -> throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
    _ -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__MULTIPLE_WORKSPACES

checkWorkspaceMembership :: WizardRequestContextC s m => U.UUID -> m ()
checkWorkspaceMembership workspaceUuid = do
  mCurrentUser <- asks (.currentUser')
  isMember <-
    case mCurrentUser of
      Just user -> isJust <$> findWorkspaceMembership' workspaceUuid user.uuid
      Nothing -> isJust <$> findWorkspaceByUuid' workspaceUuid
  unless isMember (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace"))

planeOfWorkspace :: WizardRequestContextC s m => U.UUID -> m (Maybe U.UUID)
planeOfWorkspace workspaceUuid = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  return $ if multiWorkspace then Just workspaceUuid else Nothing

checkPackagePlane :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m ()
checkPackagePlane workspaceUuid mPackageWorkspaceUuid =
  unless
    (maybe True (== workspaceUuid) mPackageWorkspaceUuid)
    (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "knowledge_model_package"))

isWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m Bool
isWorkspaceMember workspaceUuid userUuid = isJust <$> findWorkspaceMembership' workspaceUuid userUuid
