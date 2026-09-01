module Shared.Service.User.RegistrationPending.UserRegistrationPendingService (
  completeExternalRegistration,
  cleanUserRegistrationPending,
) where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserFromExternalDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import Shared.Database.DAO.User.UserRegistrationPendingDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.User.UserRegistrationPendingServiceType ()
import Shared.Localization.Messages.WizardInternal
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.User.UserRegistrationPending
import Shared.Model.User.UserRegistrationPendingServiceType
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Mail.Mailer
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantHelper
import qualified Shared.Service.User.UserOpenIdIdentityMapper as UserOpenIdIdentityMapper
import Shared.Service.User.UserRegistrationPendingService (cleanUserRegistrationPending)
import Shared.Service.User.UserService
import Shared.Service.User.UserValidation
import Shared.Service.User.WizardUserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.Login.LoginService
import Shared.Service.UserToken.Login.LoginValidation
import Shared.Util.Uuid

completeExternalRegistration :: WizardRequestContextC s m => UserFromExternalDTO -> Maybe String -> Maybe String -> m UserTokenDTO
completeExternalRegistration reqDto _mAcceptLanguages mUserAgent =
  runInTransaction $ do
    (pending :: UserRegistrationPending UserRegistrationPendingServiceType) <-
      findUserRegistrationPendingByHash reqDto.hash
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    case pending.email of
      Just idpEmail ->
        when (fmap toLower idpEmail /= fmap toLower reqDto.email) $
          throwError . UserError $
            _ERROR_VALIDATION__USER_EMAIL_FROM_IDP_CANNOT_BE_CHANGED
      Nothing -> return ()
    validateUserEmailUniqueness reqDto.email tenantUuid
    let emailVerified = case pending.email of
          Just _ -> True
          Nothing -> False
    user <- createUserForPending pending reqDto emailVerified now
    identityUuid <- liftIO generateUuid
    let identity = UserOpenIdIdentityMapper.fromPending identityUuid pending (user :: User).uuid now
    void $ insertUserOpenIdIdentity identity
    deleteUserRegistrationPendingByHash reqDto.hash
    if emailVerified
      then do
        tcAuthentication <- getCurrentTenantConfigAuthentication
        validateLoginEnabled tcAuthentication user
        createLoginToken user mUserAgent Nothing
      else do
        userEmailLink <- createUserEmailLink (user :: User).uuid RegistrationUserEmailLinkType tenantUuid
        clientUrl <- getClientUrl
        catchError
          (sendRegistrationConfirmationMail user userEmailLink.hash clientUrl)
          (\_ -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
        return EmailVerificationRequiredDTO

-- --------------------------------
-- PRIVATE
-- --------------------------------
createUserForPending
  :: WizardRequestContextC s m
  => UserRegistrationPending UserRegistrationPendingServiceType
  -> UserFromExternalDTO
  -> Bool
  -> UTCTime
  -> m User
createUserForPending pending reqDto emailVerified now = do
  checkUserLimit
  checkActiveUserLimit
  tenantUuid <- asks (.tenantUuid')
  uUuid <- liftIO generateUuid
  password <- liftIO . fmap U.toString $ generateUuid
  uPasswordHash <- generatePasswordHash password
  tcAuthentication <- getCurrentTenantConfigAuthentication
  let role = tcAuthentication.defaultRoleUuid
  uRole <- getRoleForUser role
  let user =
        fromUserExternalDTO
          uUuid
          reqDto.firstName
          reqDto.lastName
          (toLower <$> reqDto.email)
          uPasswordHash
          role
          uRole.permissions
          uRole.name
          emailVerified
          pending.imageUrl
          tenantUuid
          now
  insertUser user
  return user
