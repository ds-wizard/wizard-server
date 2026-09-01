module Shared.Service.Mail.Mailer where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Data.Aeson.Types
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadNotificationJM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import qualified Shared.Model.PersistentCommand.Mail.MailCommand as MC
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Comment.ProjectCommentThreadNotification
import Shared.Model.Project.Project
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.User
import Shared.Model.User.UserSimple
import Shared.Model.User.UserToken
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.Tenant.TenantHelper
import qualified Shared.Util.Aeson as A
import Shared.Util.JSON
import Shared.Util.Uuid

sendRegistrationConfirmationMail :: WizardRequestContextC s m => User -> String -> String -> m ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationConfirmation"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendRegistrationCreatedAnalyticsMail :: WizardRequestContextC s m => User -> m ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks (.serverConfig')
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationCreatedAnalytics"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = serverConfig.analyticalMails.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendEmailChangeMail :: WizardRequestContextC s m => User -> String -> String -> m ()
sendEmailChangeMail user hash newEmail =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "emailChange"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = newEmail}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("newEmail", A.string newEmail)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendResetPasswordMail :: WizardRequestContextC s m => UserDTO -> String -> m ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "resetPassword"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmail body user.uuid

sendTwoFactorAuthMail :: WizardRequestContextC s m => UserDTO -> String -> m ()
sendTwoFactorAuthMail user code =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "twoFactorAuth"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("code", A.string code)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmail body user.uuid

sendProjectInvitationMail :: WizardRequestContextC s m => Project -> Project -> m ()
sendProjectInvitationMail oldProject newProject =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail tcPrivacyAndSupport tcLookAndFeel tcMail clientUrl currentUser) (filter (filterPermissions currentUser) newProject.permissions)
  where
    filterPermissions :: UserDTO -> ProjectPerm -> Bool
    filterPermissions currentUser perm = perm.memberUuid /= currentUser.uuid && perm.memberUuid `notElem` fmap (.memberUuid) oldProject.permissions
    sendOneEmail tcPrivacyAndSupport tcLookAndFeel tcMail clientUrl currentUser permission =
      case permission.memberType of
        UserGroupProjectPermType -> return ()
        UserProjectPermType -> do
          user <- findUserByUuid permission.memberUuid
          let body =
                MC.MailCommand
                  { mode = "wizard"
                  , template = "projectInvitation"
                  , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
                  , parameters =
                      M.fromList
                        [ ("userUuid", A.uuid user.uuid)
                        , ("clientUrl", A.string clientUrl)
                        , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                        , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                        , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                        , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                        , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                        , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                        , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                        , ("inviteeUuid", A.uuid user.uuid)
                        , ("inviteeFirstName", A.string user.firstName)
                        , ("inviteeLastName", A.string user.lastName)
                        , ("inviteeEmail", A.string user.email)
                        , ("projectUuid", A.uuid newProject.uuid)
                        , ("projectName", A.string newProject.name)
                        , ("ownerUuid", A.uuid currentUser.uuid)
                        , ("ownerFirstName", A.string currentUser.firstName)
                        , ("ownerLastName", A.string currentUser.lastName)
                        , ("ownerEmail", A.string currentUser.email)
                        ]
                  }
          sendEmail body currentUser.uuid

sendProjectCommentThreadAssignedMail :: WizardRequestContextC s m => [ProjectCommentThreadNotification] -> m ()
sendProjectCommentThreadAssignedMail notifications =
  runInTransaction $ do
    tcMail <- findTenantConfigMail
    case notifications of
      [] -> return ()
      notification : _ -> do
        let notificationFn n =
              A.Object . KM.fromList $
                [ ("projectUuid", A.uuid n.projectUuid)
                , ("projectName", A.string n.projectName)
                , ("commentThreadUuid", A.uuid n.commentThreadUuid)
                , ("path", A.string n.path)
                , ("questionTitle", A.maybeString n.questionTitle)
                , ("resolved", A.bool n.resolved)
                , ("private", A.bool n.private)
                , ("assignedBy", A.toJSON n.assignedBy)
                , ("text", A.string n.text)
                ]
        let body =
              MC.MailCommand
                { mode = "wizard"
                , template = "commentThreadAssigned"
                , recipients = [MC.MailRecipient {uuid = Just notification.assignedTo.uuid, email = notification.assignedTo.email}]
                , parameters =
                    M.fromList
                      [ ("userFirstName", A.string notification.assignedTo.firstName)
                      , ("notifications", A.Array . Vector.fromList . fmap notificationFn $ notifications)
                      , ("clientUrl", A.string notification.clientUrl)
                      , ("appTitle", A.maybeString notification.appTitle)
                      , ("logoUrl", A.maybeString notification.logoUrl)
                      , ("primaryColor", A.maybeString notification.primaryColor)
                      , ("illustrationsColor", A.maybeString notification.illustrationsColor)
                      , ("supportEmail", A.maybeString notification.supportEmail)
                      , ("mailConfigUuid", A.maybeUuid notification.mailConfigUuid)
                      , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                      ]
                }
        sendEmailWithTenant body notification.assignedTo.uuid notification.tenantUuid

sendApiKeyCreatedMail :: WizardRequestContextC s m => UserDTO -> UserToken -> m ()
sendApiKeyCreatedMail user userToken =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyCreated"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("tokenName", A.string userToken.name)
                  , ("tokenExpiresAt", A.datetime userToken.expiresAt)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmail body user.uuid

sendApiKeyExpirationMail :: WizardRequestContextC s m => User -> UserToken -> m ()
sendApiKeyExpirationMail user userToken =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyExpiration"
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("tokenName", A.string userToken.name)
                  , ("tokenExpiresAt", A.datetime userToken.expiresAt)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  , ("mailCustomTemplates", A.bool tcMail.customTemplates)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: (WizardRequestContextC s m, ToJSON dto) => dto -> U.UUID -> m ()
sendEmail dto createdBy = do
  tenantUuid <- asks (.tenantUuid')
  sendEmailWithTenant dto createdBy tenantUuid

sendEmailWithTenant :: (WizardRequestContextC s m, ToJSON dto) => dto -> U.UUID -> U.UUID -> m ()
sendEmailWithTenant dto createdBy tenantUuid = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand uuid "mailer" "sendMail" body 10 tenantUuid (Just . U.toString $ createdBy) now
    insertPersistentCommand command
    return ()
