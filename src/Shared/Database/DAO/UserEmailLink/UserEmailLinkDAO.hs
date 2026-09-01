module Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.UserEmailLink.UserEmailLink ()
import Shared.Model.Context.RequestContext
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Logger

entityName = "user_email_link"

findUserEmailLinks :: (RequestContextC s sc m, FromField identity, FromField aType) => m [UserEmailLink identity aType]
findUserEmailLinks = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findUserEmailLinkByHash :: (RequestContextC s sc m, FromField identity, FromField aType) => String -> m (UserEmailLink identity aType)
findUserEmailLinkByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

findUserEmailLinkByHashAndType :: (RequestContextC s sc m, FromField identity, FromField aType, Show aType) => String -> aType -> m (UserEmailLink identity aType)
findUserEmailLinkByHashAndType hash aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash), ("type", show aType)]

findUserEmailLinkByIdentityAndType' :: (RequestContextC s sc m, ToField identity, FromField identity, FromField aType, Show aType) => String -> aType -> m (Maybe (UserEmailLink identity aType))
findUserEmailLinkByIdentityAndType' identity aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("type", show aType)]

findUserEmailLinkByIdentityAndHash'
  :: ( RequestContextC s sc m
     , FromField aType
     , FromField identity
     , ToField identity
     )
  => String
  -> String
  -> m (Maybe (UserEmailLink identity aType))
findUserEmailLinkByIdentityAndHash' identity hash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

insertUserEmailLink :: (RequestContextC s sc m, ToField aType, ToField identity) => UserEmailLink identity aType -> m Int64
insertUserEmailLink userEmailLink = do
  createInsertFn entityName userEmailLink

deleteUserEmailLinks :: RequestContextC s sc m => m Int64
deleteUserEmailLinks = do
  createDeleteEntitiesFn entityName

deleteUserEmailLinkByHash :: RequestContextC s sc m => String -> m Int64
deleteUserEmailLinkByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

deleteUserEmailLinkByIdentity :: RequestContextC s sc m => String -> m Int64
deleteUserEmailLinkByIdentity identity = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("identity", identity)]

deleteUserEmailLinkByIdentityAndHash :: RequestContextC s sc m => String -> String -> m Int64
deleteUserEmailLinkByIdentityAndHash identity hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

deleteUserEmailLinkOlderThen :: RequestContextC s sc m => UTCTime -> m Int64
deleteUserEmailLinkOlderThen date = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
