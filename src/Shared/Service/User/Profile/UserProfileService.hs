module Shared.Service.User.Profile.UserProfileService where

import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserSubmissionPropDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Localization.Messages.UserToken.Public
import Shared.Model.Common.SensitiveData
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserSubmissionPropEM ()
import Shared.Model.User.UserSubmissionPropList
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Mail.Mailer
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.User.Profile.UserProfileMapper
import Shared.Service.User.Profile.UserProfileValidation
import Shared.Service.User.UserService
import Shared.Service.User.UserValidation
import Shared.Service.User.WizardUserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService

getUserProfile :: WizardRequestContextC s m => m UserDTO
getUserProfile = getCurrentUser

modifyUserProfile :: WizardRequestContextC s m => UserProfileChangeDTO -> m UserDTO
modifyUserProfile reqDto = do
  currentUser <- getCurrentUser
  user <- findUserByUuid currentUser.uuid
  let newEmail = toLower <$> reqDto.email
  let emailChanged = newEmail /= user.email
  let revertPending = not emailChanged && maybe False (/= user.email) user.emailPending
  when emailChanged $ validateUserChangedEmailUniqueness reqDto.email user.email
  now <- liftIO getCurrentTime
  let updatedUser = fromUserProfileChangeDTO reqDto user revertPending now
  updateUserByUuid updatedUser
  when revertPending $ do
    mUserEmailLink :: Maybe (UserEmailLink U.UUID UserEmailLinkType) <-
      findUserEmailLinkByIdentityAndType' (U.toString currentUser.uuid) EmailChangeUserEmailLinkType
    forM_ mUserEmailLink $ \ak -> deleteUserEmailLinkByHash ak.hash
  when emailChanged $ do
    tenantUuid <- asks (.tenantUuid')
    userEmailLink <- createUserEmailLink currentUser.uuid EmailChangeUserEmailLinkType tenantUuid
    sendEmailChangeMail updatedUser userEmailLink.hash newEmail
  return . toDTO $ updatedUser

changeUserProfilePassword :: WizardRequestContextC s m => U.UUID -> UserPasswordDTO -> m ()
changeUserProfilePassword userUuid reqDto = do
  tcAuthentication <- getCurrentTenantConfigAuthentication
  user <- findUserByUuid userUuid
  when (not tcAuthentication.internal.nonAdminLoginEnabled && notElem _USERS_MANAGE_ROLE_PERMISSION user.role.permissions) $
    throwError . UserError $
      _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
  passwordHash <- generatePasswordHash reqDto.password
  now <- liftIO getCurrentTime
  updateUserPasswordByUuid userUuid passwordHash now
  return ()

getUserProfileSubmissionProps :: WizardRequestContextC s m => U.UUID -> m [UserSubmissionPropList]
getUserProfileSubmissionProps userUuid = do
  serverConfig <- asks (.serverConfig')
  submissionProps <- findUserSubmissionPropsList userUuid
  return . fmap (process serverConfig.general.secret) $ submissionProps

modifyUserProfileSubmissionProps :: WizardRequestContextC s m => [UserSubmissionPropList] -> m [UserSubmissionPropList]
modifyUserProfileSubmissionProps reqDto = do
  currentUser <- getCurrentUser
  tenantUuid <- asks (.tenantUuid')
  serverConfig <- asks (.serverConfig')
  submissionPropsEncrypted <- findUserSubmissionProps currentUser.uuid
  let submissionProps = fmap (process serverConfig.general.secret) submissionPropsEncrypted
  now <- liftIO getCurrentTime
  let submissionPropsUpdated = fromUserSubmissionPropsDTO currentUser.uuid tenantUuid submissionProps reqDto now
  let submissionPropsUpdatedEncrypted = fmap (process serverConfig.general.secret) submissionPropsUpdated
  traverse_ insertOrUpdateUserSubmissionProp submissionPropsUpdatedEncrypted
  deleteUserSubmissionPropsExcept currentUser.uuid (map (.sId) reqDto)
  return reqDto

getLocale :: WizardRequestContextC s m => m UserLocaleDTO
getLocale = do
  user <- getCurrentUser
  return . UserLocaleDTO $ user.locale

modifyLocale :: WizardRequestContextC s m => UserLocaleDTO -> m UserLocaleDTO
modifyLocale reqDto = do
  validateLocale reqDto
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  updateUserLocaleByUuid user.uuid reqDto.uuid now
  return reqDto
