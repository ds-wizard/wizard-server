module WizardServer.Service.User.ExternalIdentity.UserExternalIdentityService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.UserOpenIdIdentity
import Shared.Service.User.UserOpenIdIdentityMapper

getUserIdentities :: WizardRequestContextC s m => m [UserOpenIdIdentityDTO]
getUserIdentities = do
  currentUser <- getCurrentUser
  identities <- findUserOpenIdIdentityListsByUserUuid (currentUser :: UserDTO).uuid
  return . fmap toDTO $ identities

deleteUserIdentity :: WizardRequestContextC s m => U.UUID -> m ()
deleteUserIdentity uuid =
  runInTransaction $ do
    currentUser <- getCurrentUser
    identities <- findUserOpenIdIdentitiesByUserUuid (currentUser :: UserDTO).uuid
    unless (any (\i -> (i :: UserOpenIdIdentity).uuid == uuid) identities) $
      throwError $
        NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "user_openid_identity" [("uuid", U.toString uuid)])
    deleteUserOpenIdIdentityByUuid uuid
