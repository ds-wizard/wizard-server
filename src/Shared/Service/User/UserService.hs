module Shared.Service.User.UserService where

import Control.Monad (unless, void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Crypto.PasswordStore as PasswordStore
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Auth.AuthConsentDTO
import Shared.Api.Resource.User.UserChangeDTO
import Shared.Api.Resource.User.UserCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Localization.Messages.WizardInternal
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.SimpleFeature
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.Role
import Shared.Model.User.UserSubmissionPropEM ()
import Shared.Model.User.UserSuggestion
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Common
import Shared.Service.Mail.Mailer
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantHelper
import Shared.Service.User.UserAudit
import qualified Shared.Service.User.UserOpenIdIdentityMapper as UserOpenIdIdentityMapper
import Shared.Service.User.UserValidation
import Shared.Service.User.WizardUserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Service.UserToken.Login.LoginService
import Shared.Util.Crypto (generateRandomString)
import Shared.Util.String
import Shared.Util.Uuid

getUsersPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Pageable -> [Sort] -> m (Page UserDTO)
getUsersPage mQuery mRole pageable sort = do
  checkPermission _USERS_MANAGE_ROLE_PERMISSION
  userPage <- findUsersPage mQuery mRole pageable sort
  return . fmap toDTO $ userPage

getUserSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> m (Page UserSuggestion)
getUserSuggestionsPage = findUserSuggestionsPage

registerOrCreateUserByAdmin :: WizardRequestContextC s m => UserCreateDTO -> m UserDTO
registerOrCreateUserByAdmin reqDto =
  runInTransaction $ do
    isAdmin <- isCurrentUserAdmin
    if isAdmin
      then createUserByAdmin reqDto
      else registerUser reqDto

createUserByAdmin :: WizardRequestContextC s m => UserCreateDTO -> m UserDTO
createUserByAdmin reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    uUuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    clientUrl <- getClientUrl
    createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl False

createUserByAdminWithUuid :: WizardRequestContextC s m => UserCreateDTO -> U.UUID -> U.UUID -> String -> Bool -> m UserDTO
createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    uPasswordHash <- generatePasswordHash reqDto.password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = fromMaybe tcAuthentication.defaultRoleUuid reqDto.roleUuid
    uRole <- getRoleForUserInTenant tenantUuid role
    userDto <- createUser reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid clientUrl shouldSendRegistrationEmail
    auditUserCreateByAdmin userDto
    return userDto

registerUser :: WizardRequestContextC s m => UserCreateDTO -> m UserDTO
registerUser reqDto =
  runInTransaction $ do
    checkIfAdminIsDisabled
    checkIfRegistrationIsEnabled
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash reqDto.password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = tcAuthentication.defaultRoleUuid
    uRole <- getRoleForUser role
    clientUrl <- getClientUrl
    tenantUuid <- asks (.tenantUuid')
    mExistingUser <- findUserByEmailAndTenantUuid' (toLower reqDto.email) tenantUuid
    case mExistingUser of
      Just _ -> do
        now <- liftIO getCurrentTime
        let fakeUser = fromUserCreateDTO reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid now True
        return $ toDTO fakeUser
      Nothing -> createUser reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid clientUrl True

createUser :: WizardRequestContextC s m => UserCreateDTO -> U.UUID -> String -> U.UUID -> [String] -> String -> U.UUID -> String -> Bool -> m UserDTO
createUser reqDto uUuid uPasswordHash role uPermissions uRoleName tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    checkUserLimitForTenant tenantUuid
    checkActiveUserLimitForTenant tenantUuid
    validateUserEmailUniqueness reqDto.email tenantUuid
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash role uPermissions uRoleName tenantUuid now shouldSendRegistrationEmail
    insertUser user
    userEmailLink <- createUserEmailLink uUuid RegistrationUserEmailLinkType tenantUuid
    when
      shouldSendRegistrationEmail
      ( catchError
          (sendRegistrationConfirmationMail user userEmailLink.hash clientUrl)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
      )
    sendAnalyticsEmailIfEnabled user
    return $ toDTO user

createUserFromOpenIdLogin
  :: WizardRequestContextC s m
  => OpenIdClient
  -> String
  -> String
  -> String
  -> String
  -> Maybe String
  -> Maybe U.UUID
  -> Bool
  -> m User
createUserFromOpenIdLogin openIdClient externalId firstName lastName email mImageUrl mUserUuid active =
  runInTransaction $ do
    checkUserLimit
    checkActiveUserLimit
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    uUuid <-
      case mUserUuid of
        Just userUuid -> return userUuid
        Nothing -> liftIO generateUuid
    password <- liftIO $ generateRandomString 40
    uPasswordHash <- generatePasswordHash password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = tcAuthentication.defaultRoleUuid
    uRole <- getRoleForUser role
    let user =
          fromUserExternalDTO
            uUuid
            firstName
            lastName
            email
            uPasswordHash
            role
            uRole.permissions
            uRole.name
            active
            mImageUrl
            tenantUuid
            now
    insertUser user
    identityUuid <- liftIO generateUuid
    let identity = UserOpenIdIdentityMapper.fromCreate identityUuid externalId Nothing user.uuid openIdClient.uuid openIdClient.tenantUuid now
    _ <- insertUserOpenIdIdentity identity
    sendAnalyticsEmailIfEnabled user
    return user

getUserById :: WizardRequestContextC s m => U.UUID -> m UserDTO
getUserById userUuid = do
  user <- findUserByUuid userUuid
  return $ toDTO user

getUserDetailById :: WizardRequestContextC s m => U.UUID -> m UserDTO
getUserDetailById userUuid = do
  checkPermission _USERS_MANAGE_ROLE_PERMISSION
  getUserById userUuid

modifyUser :: WizardRequestContextC s m => U.UUID -> UserChangeDTO -> m UserDTO
modifyUser userUuid reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    user <- findUserByUuid userUuid
    when (reqDto.active && not user.active) checkActiveUserLimit
    validateUserChangedEmailUniqueness reqDto.email user.email
    (newPermissions, newRoleName) <-
      if reqDto.roleUuid /= user.role.uuid
        then do
          newRole <- getRoleForUser reqDto.roleUuid
          return (newRole.permissions, newRole.name)
        else return (user.role.permissions, user.role.name)
    updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user newPermissions newRoleName
    updateUserByUuid updatedUser
    return . toDTO $ updatedUser

changeUserPasswordByAdminOrHash :: WizardRequestContextC s m => U.UUID -> UserPasswordDTO -> Maybe String -> m ()
changeUserPasswordByAdminOrHash userUuid reqDto mHash =
  runInTransaction $ do
    isAdmin <- isCurrentUserAdmin
    if isAdmin
      then changeUserPasswordByAdmin userUuid reqDto
      else do
        let hash = fromMaybe (U.toString U.nil) mHash
        changeUserPasswordByHash userUuid hash reqDto

changeUserPasswordByAdmin :: WizardRequestContextC s m => U.UUID -> UserPasswordDTO -> m ()
changeUserPasswordByAdmin userUuid reqDto =
  runInTransaction $ do
    user <- findUserByUuid userUuid
    passwordHash <- generatePasswordHash reqDto.password
    now <- liftIO getCurrentTime
    updateUserPasswordByUuid userUuid passwordHash now
    return ()

changeUserPasswordByHash :: WizardRequestContextC s m => U.UUID -> String -> UserPasswordDTO -> m ()
changeUserPasswordByHash userUuid hash userPasswordDto =
  runInTransaction $ do
    (userEmailLink :: UserEmailLink U.UUID UserEmailLinkType) <- findUserEmailLinkByHash hash
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    passwordHash <- generatePasswordHash userPasswordDto.password
    now <- liftIO getCurrentTime
    updateUserPasswordByUuid userUuid passwordHash now
    deleteUserEmailLinkByHash userEmailLink.hash
    return ()

resetUserPassword :: WizardRequestContextC s m => UserEmailLinkDTO UserEmailLinkType -> m ()
resetUserPassword reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' (toLower reqDto.email)
    case mUser of
      Just user -> do
        tcAuthentication <- getCurrentTenantConfigAuthentication
        unless (not tcAuthentication.internal.nonAdminLoginEnabled && notElem _USERS_MANAGE_ROLE_PERMISSION user.role.permissions) $ do
          tenantUuid <- asks (.tenantUuid')
          userEmailLink <- createUserEmailLink user.uuid ForgottenPasswordUserEmailLinkType tenantUuid
          catchError
            (sendResetPasswordMail (toDTO user) userEmailLink.hash)
            (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)
      Nothing -> return ()

changeUserState :: WizardRequestContextC s m => String -> Bool -> m ()
changeUserState hash active =
  runInTransaction $ do
    checkActiveUserLimit
    (userEmailLink :: UserEmailLink U.UUID UserEmailLinkType) <- findUserEmailLinkByHash hash
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    now <- liftIO getCurrentTime
    let baseUser :: User
        baseUser = user {active = active, updatedAt = now}
    let updatedUser :: User
        updatedUser =
          case (userEmailLink.aType, user.emailPending) of
            (RegistrationUserEmailLinkType, Just pendingEmail) ->
              baseUser
                { email = pendingEmail
                , emailVerifiedAt = Just now
                , emailPending = Nothing
                }
            (ConsentsRequiredUserEmailLinkType, Just pendingEmail) ->
              baseUser
                { email = pendingEmail
                , emailVerifiedAt = Just now
                , emailPending = Nothing
                }
            _ -> baseUser
    updateUserByUuid updatedUser
    void $ deleteUserEmailLinkByHash userEmailLink.hash

confirmEmailChange :: WizardRequestContextC s m => String -> m ()
confirmEmailChange hash =
  runInTransaction $ do
    (userEmailLink :: UserEmailLink U.UUID UserEmailLinkType) <- findUserEmailLinkByHashAndType hash EmailChangeUserEmailLinkType
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    now <- liftIO getCurrentTime
    case user.emailPending of
      Just newEmail -> do
        validateUserEmailUniqueness newEmail user.tenantUuid
        let updatedUser :: User
            updatedUser =
              user
                { email = newEmail
                , emailPending = Nothing
                , emailVerifiedAt = Just now
                , updatedAt = now
                }
        updateUserByUuid updatedUser
        void $ deleteUserEmailLinkByHash userEmailLink.hash
      Nothing -> void $ deleteUserEmailLinkByHash userEmailLink.hash

confirmConsents :: WizardRequestContextC s m => AuthConsentDTO -> Maybe String -> m UserTokenDTO
confirmConsents reqDto mUserAgent = do
  (userEmailLink :: UserEmailLink U.UUID UserEmailLinkType) <- findUserEmailLinkByHash reqDto.hash
  validateUserEmailLinkNotExpired userEmailLink
  user <- findUserByUuid userEmailLink.identity
  changeUserState reqDto.hash True
  createLoginToken user mUserAgent reqDto.sessionState

deleteUser :: WizardRequestContextC s m => U.UUID -> m ()
deleteUser userUuid =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    _ <- findUserByUuid userUuid
    void $ deleteUserByUuid userUuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getRoleForUser :: WizardRequestContextC s m => U.UUID -> m Role
getRoleForUser roleUuid = do
  tenantUuid <- asks (.tenantUuid')
  getRoleForUserInTenant tenantUuid roleUuid

getRoleForUserInTenant :: WizardRequestContextC s m => U.UUID -> U.UUID -> m Role
getRoleForUserInTenant tenantUuid roleUuid = findRoleByUuidAndTenant roleUuid tenantUuid

generatePasswordHash :: WizardRequestContextC s m => String -> m String
generatePasswordHash password = do
  hash <- liftIO $ BS.unpack <$> PasswordStore.makePasswordWith PasswordStore.pbkdf2 (BS.pack password) 17
  return $ "pbkdf2:" ++ hash

updateUserTimestamp :: WizardRequestContextC s m => User -> m User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user {updatedAt = now}

sendAnalyticsEmailIfEnabled :: WizardRequestContextC s m => User -> m ()
sendAnalyticsEmailIfEnabled user = do
  serverConfig <- asks (.serverConfig')
  when serverConfig.analyticalMails.enabled (sendRegistrationCreatedAnalyticsMail user)

checkIfRegistrationIsEnabled :: WizardRequestContextC s m => m ()
checkIfRegistrationIsEnabled = checkIfTenantFeatureIsEnabled "Registration" getCurrentTenantConfigAuthentication (.internal.registration.enabled)

checkIfAdminIsDisabled :: WizardRequestContextC s m => m ()
checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "User Management Endpoints" (\s -> not s.admin.enabled)
