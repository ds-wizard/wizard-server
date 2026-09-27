module Shared.Service.Acl.LibraryAcl where

import Control.Monad (unless)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext

checkLibraryPermission :: WizardRequestContextC s m => String -> Maybe U.UUID -> m ()
checkLibraryPermission permission mWorkspaceUuid = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  case mWorkspaceUuid of
    Nothing | multiWorkspace -> checkPermissionInWorkspace _ORGANIZATION_LIBRARY_MANAGE_ROLE_PERMISSION Nothing
    _ -> checkPermissionInWorkspace permission mWorkspaceUuid

checkLibraryPermissionAnywhere :: WizardRequestContextC s m => String -> m ()
checkLibraryPermissionAnywhere permission = do
  multiWorkspace <- asks (.tenantMultiWorkspace')
  managesLibrary <- hasPermission _ORGANIZATION_LIBRARY_MANAGE_ROLE_PERMISSION
  unless (multiWorkspace && managesLibrary) (checkPermission permission)
