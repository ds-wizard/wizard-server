module WizardServer.Service.User.UserInvitationService where

import Control.Monad.Reader (asks, liftIO)

import Shared.Api.Resource.Mcp.User.McpUserInviteDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Mail.Mailer
import Shared.Service.User.UserService
import Shared.Service.User.WizardUserMapper
import Shared.Service.UserEmailLink.WizardUserEmailLinkService
import Shared.Util.Crypto (generateRandomString)

inviteUsers :: WizardRequestContextC s m => [McpUserInviteDTO] -> m [UserDTO]
inviteUsers reqDtos = runInTransaction $ traverse inviteUser reqDtos

inviteUser :: WizardRequestContextC s m => McpUserInviteDTO -> m UserDTO
inviteUser reqDto = do
  password <- liftIO $ generateRandomString 40
  user <- createUserByAdmin (fromMcpUserInviteDTO reqDto password)
  tenantUuid <- asks (.tenantUuid')
  userEmailLink <- createUserEmailLink user.uuid ForgottenPasswordUserEmailLinkType tenantUuid
  sendResetPasswordMail user userEmailLink.hash
  return user
