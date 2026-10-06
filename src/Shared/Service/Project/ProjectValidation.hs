module Shared.Service.Project.ProjectValidation where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Text.Regex.TDFA

import Shared.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.User.UserGroup
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation
import Shared.Service.Workspace.WorkspaceScopeService

validateProjectSettingsChangeDTO :: WizardRequestContextC s m => U.UUID -> ProjectSettingsChangeDTO -> m ()
validateProjectSettingsChangeDTO workspaceUuid reqDto = do
  validateProjectTags reqDto.projectTags
  forM_ reqDto.documentTemplateUuid $ \dtUuid -> do
    tml <- findDocumentTemplateByUuid dtUuid
    checkTemplateWorkspace workspaceUuid tml.workspaceUuid
    validateLanguageAvailability tml reqDto.documentTemplateLanguage

validateProjectShareChangeDTO :: WizardRequestContextC s m => U.UUID -> ProjectShareChangeDTO -> m ()
validateProjectShareChangeDTO workspaceUuid reqDto = forM_ reqDto.permissions validateMember
  where
    validateMember perm =
      case perm.memberType of
        UserProjectPermType -> do
          isMember <- isWorkspaceMember workspaceUuid perm.memberUuid
          unless isMember (throwError $ UserError _ERROR_SERVICE_WORKSPACE__NOT_MEMBER)
        UserGroupProjectPermType -> do
          userGroup <- findUserGroupByUuid perm.memberUuid
          unless (userGroup.workspaceUuid == workspaceUuid) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__NOT_MEMBER)

validateProjectTags :: WizardRequestContextC s m => [String] -> m ()
validateProjectTags = traverse_ validateProjectTag

validateProjectTag :: WizardRequestContextC s m => String -> m ()
validateProjectTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if tag =~ "^[^,]+$"
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])
