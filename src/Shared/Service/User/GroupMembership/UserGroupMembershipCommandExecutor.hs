module Shared.Service.User.GroupMembership.UserGroupMembershipCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.User.RemoveUserGroupMembersCommand
import Shared.Service.User.GroupMembership.UserGroupMembershipService
import Shared.Util.Logger

cComponent = "user_group_membership"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cRemoveMembersName = cRemoveMembers command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cRemoveMembersName = "removeMembers"

cRemoveMembers :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cRemoveMembers persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String RemoveUserGroupMembersCommand
  case eCommand of
    Right command -> do
      removeUserGroupMembersFromOnlineUsers command.userGroupUuid command.userUuids
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
