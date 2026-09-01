module WizardServer.Service.User.Role.RoleValidation where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission

validateRoleChangeDTO :: WizardRequestContextC s m => RoleChangeDTO -> m ()
validateRoleChangeDTO dto = do
  when (null dto.name) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_NAME_EMPTY)
  forM_ dto.permissions $ \permission ->
    unless (permission `elem` assignableRolePermissions) (throwError . UserError $ _ERROR_VALIDATION__USER_ROLE_INVALID_PERMISSION permission)
