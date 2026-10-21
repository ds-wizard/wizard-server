module Shared.Service.Mcp.TenantContext.TenantContextService where

import Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as A
import qualified Data.UUID as U

import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Acl.AclService (effectivePermissions)
import Shared.Service.Mcp.TenantContext.TenantContextMapper
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Service.Workspace.WorkspaceService

getTenantContext :: WizardRequestContextC s m => m A.Value -> m TenantContextDTO
getTenantContext getUsage = do
  mWorkspaceUuid <- resolveContextWorkspace
  user <- getCurrentUser
  context <- ask
  workspaces <- getAccessibleWorkspaces
  mUsage <- getUsageIfPermitted getUsage
  settings <- getContextSettings mWorkspaceUuid
  let permissions = effectivePermissions user context.workspaceRoles' context.tenantMultiWorkspace' context.scope'
  let workspaceDtos = fmap (toWorkspaceDTO context.workspaceRoles') workspaces
  return $ toTenantContextDTO user permissions context.tenantMultiWorkspace' mWorkspaceUuid workspaceDtos mUsage settings

resolveContextWorkspace :: WizardRequestContextC s m => m (Maybe U.UUID)
resolveContextWorkspace = do
  scope <- asks (.scope')
  case scope of
    WorkspaceScope _ -> Just <$> requireWorkspaceScope
    _ -> return Nothing

getUsageIfPermitted :: WizardRequestContextC s m => m A.Value -> m (Maybe A.Value)
getUsageIfPermitted getUsage = do
  permitted <- hasPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  if permitted then Just <$> getUsage else return Nothing

getContextSettings :: WizardRequestContextC s m => Maybe U.UUID -> m TenantContextSettingsDTO
getContextSettings mWorkspaceUuid =
  toSettingsDTO
    <$> getEffectiveSettingsDashboardAndMenu mWorkspaceUuid
    <*> getEffectiveSettingsProjects mWorkspaceUuid
    <*> getEffectiveSettingsSupport mWorkspaceUuid
    <*> getEffectiveSettingsSubmission mWorkspaceUuid
