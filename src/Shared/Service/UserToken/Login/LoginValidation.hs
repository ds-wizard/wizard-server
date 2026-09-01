module Shared.Service.UserToken.Login.LoginValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Localization.Messages.UserToken.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.User.UserUtil

validate :: WizardRequestContextC s m => LoginDTO -> User -> m ()
validate reqDto user = do
  validateIsUserActive user
  validateUserPassword reqDto user

validateLoginEnabled :: WizardRequestContextC s m => TenantConfigAuthentication -> User -> m ()
validateLoginEnabled tcInternalAuthentication user = do
  role <- findRoleByUuid user.role.uuid
  when (not tcInternalAuthentication.internal.nonAdminLoginEnabled && not role.isAdmin) $
    throwError . UserError $
      _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

validateIsUserActive :: WizardRequestContextC s m => User -> m ()
validateIsUserActive user =
  if user.active
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED

validateUserPassword :: WizardRequestContextC s m => LoginDTO -> User -> m ()
validateUserPassword reqDto user =
  if verifyPassword reqDto.password user.passwordHash
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

validateCode :: WizardRequestContextC s m => User -> Int -> TenantConfigAuthentication -> m ()
validateCode user code tcAuthentication = do
  (mUserEmailLink :: Maybe (UserEmailLink U.UUID UserEmailLinkType)) <- findUserEmailLinkByIdentityAndHash' (U.toString user.uuid) (show code)
  case mUserEmailLink of
    Just userEmailLink -> do
      let timeDelta = realToFrac . toInteger $ tcAuthentication.internal.twoFactorAuth.expiration
      now <- liftIO getCurrentTime
      when (addUTCTime timeDelta userEmailLink.createdAt < now) (throwError $ UserError _ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED)
    Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_CODE
