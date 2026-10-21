module Shared.Service.User.UserNotification where

import Control.Monad (when)
import Control.Monad.Reader (ask, liftIO)
import Database.PostgreSQL.Simple.Transaction (newSavepoint, releaseSavepoint, rollbackToSavepoint)

import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Component
import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.RoleSimple
import Shared.Model.User.User
import Shared.Service.Mail.Mailer
import Shared.Util.Error (tryError)
import Shared.Util.Exception
import Shared.Util.Logger

notifyTenantRoleChanged :: WizardRequestContextC s m => RoleSimple -> User -> m ()
notifyTenantRoleChanged oldRole user =
  swallowMailError . whenUserNotified user $
    when (oldRole.uuid /= user.role.uuid) (sendTenantRoleChangedMail user oldRole user.role)

whenUserNotified :: WizardRequestContextC s m => User -> m () -> m ()
whenUserNotified user action = do
  currentUser <- getCurrentUser
  when (user.active && user.uuid /= currentUser.uuid) action

swallowMailError :: forall s m. WizardRequestContextC s m => m () -> m ()
swallowMailError action = do
  savepoint <- runDB newSavepoint
  context <- ask
  result <- liftIO . tryNonAsync $ runRequestContextWithRequestContext @s @m (tryError action) context
  case result of
    Right (Right (Right ())) -> runDB (`releaseSavepoint` savepoint)
    failure -> do
      runDB (`rollbackToSavepoint` savepoint)
      logWarnI _CMP_SERVICE ("Notification mail not sent: " ++ show failure)
