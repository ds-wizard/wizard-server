module Shared.Model.Context.RequestContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import qualified Data.UUID as U
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.User.RoleDAO
import Shared.Localization.Messages.User.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import qualified Shared.Model.User.Role as Role
import Shared.Model.User.RoleSimple

getCurrentUser :: WizardRequestContextC s m => m UserDTO
getCurrentUser = do
  mCurrentUser <- asks (.currentUser')
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

getCurrentUserUuid :: WizardRequestContextC s m => m (Maybe U.UUID)
getCurrentUserUuid = do
  mCurrentUser <- asks (.currentUser')
  return . fmap (.uuid) $ mCurrentUser

isCurrentUserAdmin :: WizardRequestContextC s m => m Bool
isCurrentUserAdmin = do
  mUser <- asks (.currentUser')
  case mUser of
    Just user -> do
      role <- findRoleByUuid user.role.uuid
      return role.isAdmin
    Nothing -> return False
