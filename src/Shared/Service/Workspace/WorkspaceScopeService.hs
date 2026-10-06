module Shared.Service.Workspace.WorkspaceScopeService where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Model.User.RolePermission
import Shared.Model.User.RoleSimple
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
      checkWorkspaceReachable workspaceUuid
      return workspaceUuid
    NoScope ->
      if multiWorkspace
        then throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED
        else do
          workspaceUuid <- getOnlyWorkspaceUuid
          checkWorkspaceReachable workspaceUuid
          return workspaceUuid

requireTenantOrWorkspaceScope :: WizardRequestContextC s m => m (Maybe U.UUID)
requireTenantOrWorkspaceScope = do
  scope <- asks (.scope')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  case scope of
    TenantScope -> return Nothing
    WorkspaceScope workspaceUuid -> do
      unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
      checkWorkspaceReachable workspaceUuid
      return $ Just workspaceUuid
    NoScope ->
      if multiWorkspace
        then throwError $ UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED
        else return Nothing

checkWorkspacePlaneAvailable :: WizardRequestContextC s m => m ()
checkWorkspacePlaneAvailable = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE)

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

checkWorkspaceReachable :: WizardRequestContextC s m => U.UUID -> m ()
checkWorkspaceReachable workspaceUuid = do
  reachable <- isWorkspaceReachable workspaceUuid
  mWorkspace <- findWorkspaceByUuid' workspaceUuid
  unless (isJust mWorkspace && reachable) (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace"))

isWorkspaceReachable :: WizardRequestContextC s m => U.UUID -> m Bool
isWorkspaceReachable workspaceUuid = do
  mCurrentUser <- asks (.currentUser')
  workspaceRoles <- asks (.workspaceRoles')
  case mCurrentUser of
    Nothing -> return True
    Just user
      | M.member workspaceUuid workspaceRoles || any (`elem` user.role.permissions) workspaceReachRolePermissions -> return True
      | otherwise -> isWorkspaceMember workspaceUuid user.uuid

ownerWorkspaceUuid :: WizardRequestContextC s m => U.UUID -> m (Maybe U.UUID)
ownerWorkspaceUuid workspaceUuid = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  return $ if multiWorkspace then Just workspaceUuid else Nothing

checkPackageWorkspace :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m ()
checkPackageWorkspace = checkWorkspaceMatch "knowledge_model_package"

checkTemplateWorkspace :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m ()
checkTemplateWorkspace = checkWorkspaceMatch "document_template"

checkTemplateInWorkspace :: WizardRequestContextC s m => Maybe U.UUID -> Maybe U.UUID -> m ()
checkTemplateInWorkspace (Just workspaceUuid) mTemplateWorkspaceUuid = checkTemplateWorkspace workspaceUuid mTemplateWorkspaceUuid
checkTemplateInWorkspace Nothing mTemplateWorkspaceUuid =
  unless (isNothing mTemplateWorkspaceUuid) (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "document_template"))

checkTemplateWorkspaceByUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
checkTemplateWorkspaceByUuid workspaceUuid dtUuid = findDocumentTemplateByUuid dtUuid >>= checkTemplateWorkspace workspaceUuid . (.workspaceUuid)

checkWorkspaceMatch :: WizardRequestContextC s m => String -> U.UUID -> Maybe U.UUID -> m ()
checkWorkspaceMatch entityName workspaceUuid mEntityWorkspaceUuid =
  unless
    (maybe True (== workspaceUuid) mEntityWorkspaceUuid)
    (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE entityName))

isWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m Bool
isWorkspaceMember workspaceUuid userUuid = isJust <$> findWorkspaceMembership' workspaceUuid userUuid
