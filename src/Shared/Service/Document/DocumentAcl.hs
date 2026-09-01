module Shared.Service.Document.DocumentAcl where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Service.Project.ProjectAcl

checkViewPermissionToDoc :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkViewPermissionToDoc mProjectUuid = do
  case mProjectUuid of
    Just projectUuid -> do
      project <- findProjectByUuid projectUuid
      checkViewPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Read Document"

checkViewPermissionToDoc' :: WizardRequestContextC s m => Project -> m ()
checkViewPermissionToDoc' project = checkViewPermissionToProject project.visibility project.sharing project.permissions

checkEditPermissionToDoc :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkEditPermissionToDoc mProjectUuid = do
  case mProjectUuid of
    Just projectUuid -> do
      _ <- getCurrentUser
      project <- findProjectByUuid projectUuid
      checkEditPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Document"
