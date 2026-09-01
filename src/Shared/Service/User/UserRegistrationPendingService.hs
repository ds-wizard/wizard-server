module Shared.Service.User.UserRegistrationPendingService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.DAO.Common
import Shared.Database.DAO.User.UserRegistrationPendingDAO
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserRegistrationPending
import Shared.Util.Date (nominalDayInSeconds)
import Shared.Util.Logger
import Shared.Util.Uuid

upsertPendingExternalRegistration
  :: ( RequestContextC s sc m
     , ToField serviceType
     , FromField serviceType
     , Show serviceType
     )
  => serviceType
  -> U.UUID
  -> String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> m (UserRegistrationPending serviceType)
upsertPendingExternalRegistration serviceType providerUuid externalId mExternalLabel mEmail mFirstName mLastName mImageUrl mAffiliation =
  runInTransaction logInfoI logWarnI $ do
    tenantUuid <- asks (.tenantUuid')
    mExisting <-
      findUserRegistrationPendingByServiceTypeAndExternalIdAndProviderUuid' serviceType externalId providerUuid
    case mExisting of
      Just existing -> do
        let updated =
              existing
                { externalLabel = mExternalLabel
                , email = mEmail
                , firstName = mFirstName
                , lastName = mLastName
                , imageUrl = mImageUrl
                , affiliation = mAffiliation
                }
        void $ updateUserRegistrationPendingByUuid updated
        return updated
      Nothing -> do
        uuid <- liftIO generateUuid
        hashUuid <- liftIO generateUuid
        now <- liftIO getCurrentTime
        let pending =
              UserRegistrationPending
                { uuid = uuid
                , hash = U.toString hashUuid
                , serviceType = serviceType
                , providerUuid = providerUuid
                , externalId = externalId
                , externalLabel = mExternalLabel
                , email = mEmail
                , firstName = mFirstName
                , lastName = mLastName
                , imageUrl = mImageUrl
                , affiliation = mAffiliation
                , tenantUuid = tenantUuid
                , createdAt = now
                }
        void $ insertUserRegistrationPending pending
        return pending

cleanUserRegistrationPending :: RequestContextC s sc m => m ()
cleanUserRegistrationPending = do
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ nominalDayInSeconds * (-1)
  let dayBefore = addUTCTime timeDelta now
  void $ deleteUserRegistrationPendingsOlderThan dayBefore
