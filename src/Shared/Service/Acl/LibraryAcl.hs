module Shared.Service.Acl.LibraryAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Library.LibraryDependents

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

checkDeleteAllowed :: WizardRequestContextC s m => LibraryDependents -> m ()
checkDeleteAllowed dependents =
  unless dependents.deleteAllowed $
    throwError . UserError $
      _ERROR_SERVICE_LIBRARY__DELETE_HIDDEN_DEPENDENTS
        dependents.hidden.knowledgeModelPackages
        dependents.hidden.editors
        dependents.hidden.projects
        dependents.hidden.documents
        dependents.hidden.workspaces
