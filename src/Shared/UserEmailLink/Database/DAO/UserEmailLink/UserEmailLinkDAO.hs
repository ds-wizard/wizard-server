module Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.UserEmailLink.Database.Mapping.UserEmailLink.UserEmailLink ()
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

entityName = "user_email_link"

findUserEmailLinks :: (AppContextC s sc m, FromField identity, FromField aType) => m [UserEmailLink identity aType]
findUserEmailLinks = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid]

findUserEmailLinkByHash :: (AppContextC s sc m, FromField identity, FromField aType) => String -> m (UserEmailLink identity aType)
findUserEmailLinkByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("hash", hash)]

findUserEmailLinkByHashAndType :: (AppContextC s sc m, FromField identity, FromField aType, Show aType) => String -> aType -> m (UserEmailLink identity aType)
findUserEmailLinkByHashAndType hash aType = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("hash", hash), ("type", show aType)]

findUserEmailLinkByIdentityAndType' :: (AppContextC s sc m, ToField identity, FromField identity, FromField aType, Show aType) => String -> aType -> m (Maybe (UserEmailLink identity aType))
findUserEmailLinkByIdentityAndType' identity aType = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("identity", identity), ("type", show aType)]

findUserEmailLinkByIdentityAndHash'
  :: ( AppContextC s sc m
     , FromField aType
     , FromField identity
     , ToField identity
     )
  => String
  -> String
  -> m (Maybe (UserEmailLink identity aType))
findUserEmailLinkByIdentityAndHash' identity hash = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

insertUserEmailLink :: (AppContextC s sc m, ToField aType, ToField identity) => UserEmailLink identity aType -> m Int64
insertUserEmailLink userEmailLink = do
  table <- tableName entityName
  createInsertFn table userEmailLink

deleteUserEmailLinks :: AppContextC s sc m => m Int64
deleteUserEmailLinks = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteUserEmailLinkByHash :: AppContextC s sc m => String -> m Int64
deleteUserEmailLinkByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("hash", hash)]

deleteUserEmailLinkByIdentity :: AppContextC s sc m => String -> m Int64
deleteUserEmailLinkByIdentity identity = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("identity", identity)]

deleteUserEmailLinkByIdentityAndHash :: AppContextC s sc m => String -> String -> m Int64
deleteUserEmailLinkByIdentityAndHash identity hash = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

deleteUserEmailLinkOlderThen :: AppContextC s sc m => UTCTime -> m Int64
deleteUserEmailLinkOlderThen date = do
  table <- tableName entityName
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [table]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
