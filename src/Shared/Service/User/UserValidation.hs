module Shared.Service.User.UserValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Database.DAO.User.UserDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error

validateUserEmailUniqueness :: WizardRequestContextC s m => String -> U.UUID -> m ()
validateUserEmailUniqueness email tenantUuid = do
  mUserFromDb <- findUserByEmailAndTenantUuid' (toLower <$> email) tenantUuid
  case mUserFromDb of
    Just _ -> throwError $ ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email])
    Nothing -> return ()

validateUserChangedEmailUniqueness :: WizardRequestContextC s m => String -> String -> m ()
validateUserChangedEmailUniqueness newEmail oldEmail = do
  tenantUuid <- asks (.tenantUuid')
  when (fmap toLower newEmail /= fmap toLower oldEmail) (validateUserEmailUniqueness newEmail tenantUuid)
