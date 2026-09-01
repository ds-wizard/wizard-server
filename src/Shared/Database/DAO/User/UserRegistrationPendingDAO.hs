module Shared.Database.DAO.User.UserRegistrationPendingDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.UserRegistrationPending ()
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserRegistrationPending
import Shared.Util.String

entityName = "user_registration_pending"

findUserRegistrationPendingByHash
  :: (RequestContextC s sc m, FromField serviceType) => String -> m (UserRegistrationPending serviceType)
findUserRegistrationPendingByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

findUserRegistrationPendingByServiceTypeAndExternalIdAndProviderUuid'
  :: (RequestContextC s sc m, FromField serviceType, Show serviceType)
  => serviceType
  -> String
  -> U.UUID
  -> m (Maybe (UserRegistrationPending serviceType))
findUserRegistrationPendingByServiceTypeAndExternalIdAndProviderUuid' serviceType externalId providerUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn'
    entityName
    [ tenantQueryUuid tenantUuid
    , ("service_type", show serviceType)
    , ("external_id", externalId)
    , ("provider_uuid", U.toString providerUuid)
    ]

insertUserRegistrationPending
  :: (RequestContextC s sc m, ToField serviceType) => UserRegistrationPending serviceType -> m Int64
insertUserRegistrationPending pending = do
  createInsertFn entityName pending

updateUserRegistrationPendingByUuid
  :: (RequestContextC s sc m, ToField serviceType) => UserRegistrationPending serviceType -> m Int64
updateUserRegistrationPendingByUuid pending = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, hash = ?, service_type = ?, provider_uuid = ?, external_id = ?, external_label = ?, email = ?, first_name = ?, last_name = ?, image_url = ?, affiliation = ?, tenant_uuid = ?, created_at = ? WHERE uuid = ? AND tenant_uuid = ?"
            [entityName]
  let params = toRow pending ++ [toField pending.uuid, toField pending.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserRegistrationPendings :: RequestContextC s sc m => m Int64
deleteUserRegistrationPendings = do
  createDeleteEntitiesFn entityName

deleteUserRegistrationPendingByUuid :: RequestContextC s sc m => U.UUID -> m ()
deleteUserRegistrationPendingByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()

deleteUserRegistrationPendingByHash :: RequestContextC s sc m => String -> m ()
deleteUserRegistrationPendingByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]
  return ()

deleteUserRegistrationPendingsOlderThan :: RequestContextC s sc m => UTCTime -> m Int64
deleteUserRegistrationPendingsOlderThan threshold = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ?" [entityName]
  let params = [toField threshold]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
