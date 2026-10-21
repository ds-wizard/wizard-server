module Shared.Service.Workspace.WorkspaceNotification where

import Control.Monad (when)

import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import Shared.Service.Mail.Mailer
import Shared.Service.User.UserNotification

notifyWorkspaceMemberSaved :: WizardRequestContextC s m => Workspace -> User -> Maybe WorkspaceMembership -> WorkspaceMembership -> m ()
notifyWorkspaceMemberSaved workspace user Nothing membership =
  swallowMailError . whenUserNotified user $
    sendWorkspaceMemberAddedMail user workspace =<< findRoleByUuid membership.roleUuid
notifyWorkspaceMemberSaved workspace user (Just oldMembership) membership =
  swallowMailError . whenUserNotified user $
    when (oldMembership.roleUuid /= membership.roleUuid) $ do
      oldRole <- findRoleByUuid oldMembership.roleUuid
      newRole <- findRoleByUuid membership.roleUuid
      sendWorkspaceMemberRoleChangedMail user workspace oldRole newRole

notifyWorkspaceMemberRemoved :: WizardRequestContextC s m => Workspace -> WorkspaceMembership -> m ()
notifyWorkspaceMemberRemoved workspace membership =
  swallowMailError $ do
    user <- findUserByUuid membership.userUuid
    whenUserNotified user $
      sendWorkspaceMemberRemovedMail user workspace =<< findRoleByUuid membership.roleUuid
