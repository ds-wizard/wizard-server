module Shared.Service.UserEmailLink.UserEmailLinkService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Database.PostgreSQL.Simple.ToField
import Shared.Database.DAO.Common
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.Context.RequestContext
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Date
import Shared.Util.Logger
import Shared.Util.Uuid

createUserEmailLink
  :: (RequestContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> m (UserEmailLink identity aType)
createUserEmailLink identity actionType tenantUuid = do
  hash <- liftIO generateUuid
  createUserEmailLinkWithHash identity actionType tenantUuid (U.toString hash)

createUserEmailLinkWithHash
  :: (RequestContextC s sc m, ToField aType, ToField identity)
  => identity
  -> aType
  -> U.UUID
  -> String
  -> m (UserEmailLink identity aType)
createUserEmailLinkWithHash identity actionType tenantUuid hash =
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userEmailLink =
          UserEmailLink
            { uuid = uuid
            , identity = identity
            , aType = actionType
            , hash = hash
            , tenantUuid = tenantUuid
            , createdAt = now
            }
    insertUserEmailLink userEmailLink
    return userEmailLink

cleanUserEmailLinks :: RequestContextC s sc m => m ()
cleanUserEmailLinks = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  deleteUserEmailLinkOlderThen dayBefore
  return ()
