module Shared.Service.UserEmailLink.WizardUserEmailLinkService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.DAO.UserEmailLink.WizardUserEmailLinkDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.Tenant.Config.ConfigService
import Shared.Util.Uuid

createUserEmailLink :: WizardRequestContextC s m => U.UUID -> UserEmailLinkType -> U.UUID -> m (UserEmailLink U.UUID UserEmailLinkType)
createUserEmailLink userUuid actionType tenantUuid = do
  hash <- liftIO generateUuid
  createUserEmailLinkWithHash userUuid actionType tenantUuid (U.toString hash)

createUserEmailLinkWithHash :: WizardRequestContextC s m => U.UUID -> UserEmailLinkType -> U.UUID -> String -> m (UserEmailLink U.UUID UserEmailLinkType)
createUserEmailLinkWithHash userUuid actionType tenantUuid hash =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userEmailLink =
          UserEmailLink
            { uuid = uuid
            , identity = userUuid
            , aType = actionType
            , hash = hash
            , tenantUuid = tenantUuid
            , createdAt = now
            }
    insertUserEmailLink userEmailLink
    return userEmailLink

cleanUserEmailLinks :: WizardRequestContextC s m => m ()
cleanUserEmailLinks = void deleteUserEmailLinksExpiredByTenantConfig

validateUserEmailLinkNotExpired :: WizardRequestContextC s m => UserEmailLink identity aType -> m ()
validateUserEmailLinkNotExpired userEmailLink = do
  tcAuthentication <- getTenantConfigAuthenticationByUuid userEmailLink.tenantUuid
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ tcAuthentication.internal.userEmailLinkExpiration * 3600
  when (addUTCTime timeDelta userEmailLink.createdAt < now) (throwError $ UserError _ERROR_SERVICE_USER_EMAIL_LINK__EXPIRED)
