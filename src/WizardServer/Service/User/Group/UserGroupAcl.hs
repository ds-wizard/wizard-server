module WizardServer.Service.User.Group.UserGroupAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.User.UserDTO
import Shared.Localization.Messages.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.UserGroup

checkViewPermission :: (WizardRequestContextC s m, HasField "uuid" user U.UUID) => UserGroup -> [user] -> m ()
checkViewPermission userGroup users = do
  currentUser <- getCurrentUser
  let userUuids = fmap (.uuid) users
  hasPermission <- hasPermission _USERS_MANAGE_ROLE_PERMISSION
  unless
    (hasPermission || not userGroup.private || currentUser.uuid `elem` userUuids)
    (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View UserGroup")
