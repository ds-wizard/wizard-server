module Shared.Database.DAO.User.UserTokenDAO where

import Control.Monad.Reader (asks)
import qualified Data.Cache as C
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.UserToken ()
import Shared.Database.Mapping.User.UserTokenList ()
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserToken
import Shared.Model.User.UserTokenList
import Shared.Service.Cache.UserTokenCache
import Shared.Util.Cache
import Shared.Util.Logger
import Shared.Util.String

entityName = "user_token"

pageLabel = "tokens"

findUserTokens :: RequestContextC s sc m => m [UserToken]
findUserTokens = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findUserTokensByUserUuidAndTypeAndTokenUuid :: RequestContextC s sc m => U.UUID -> UserTokenType -> Maybe U.UUID -> m [UserTokenList]
findUserTokensByUserUuidAndTypeAndTokenUuid userUuid tokenType mCurrentTokenUuid = do
  tenantUuid <- asks (.tenantUuid')
  let currentSessionCondition =
        case mCurrentTokenUuid of
          Just _ -> "uuid = ?"
          Nothing -> "false"
  let sql =
        fromString $
          f'
            "SELECT uuid, name, user_agent, %s as current_session, expires_at, created_at \
            \FROM %s \
            \WHERE tenant_uuid = ? AND user_uuid = ? AND type = ?"
            [currentSessionCondition, entityName]
  let params = (fmap U.toString . maybeToList $ mCurrentTokenUuid) ++ [U.toString tenantUuid, U.toString userUuid, show tokenType]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findApiUserTokensWithCloseExpiration :: RequestContextC s sc m => m [UserToken]
findApiUserTokensWithCloseExpiration = do
  let sql =
        f'
          "SELECT * \
          \FROM %s \
          \WHERE type = '%s' AND ((now() + interval '1 day' < expires_at AND expires_at < now() + interval '2 day') OR (now() + interval '6 day' < expires_at AND expires_at < now() + interval '7 day'))"
          [entityName, show ApiKeyUserTokenType]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findUserTokensByUserUuid :: RequestContextC s sc m => U.UUID -> m [UserToken]
findUserTokensByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserTokensByUserUuidAndType :: RequestContextC s sc m => U.UUID -> String -> m [UserToken]
findUserTokensByUserUuidAndType userUuid aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("type", aType)]

findUserTokensBySessionState :: RequestContextC s sc m => String -> m [UserToken]
findUserTokensBySessionState sessionState = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("session_state", sessionState)]

findUserTokensByValue :: RequestContextC s sc m => String -> m [UserToken]
findUserTokensByValue value = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("value", value)]

findUserTokensByTenantUuid :: RequestContextC s sc m => U.UUID -> m [UserToken]
findUserTokensByTenantUuid tenantUuid = do
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findUserTokenByUuid
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => U.UUID
  -> m UserToken
findUserTokenByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      tenantUuid <- asks (.tenantUuid')
      createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", uuid)]

insertUserToken
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => UserToken
  -> m Int64
insertUserToken userToken = do
  result <- createInsertFn entityName userToken
  addToCache userToken
  return result

deleteUserTokens :: RequestContextC s sc m => m Int64
deleteUserTokens = do
  createDeleteEntitiesFn entityName

deleteUserTokensWithExpiration :: RequestContextC s sc m => m Int64
deleteUserTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteUserTokenByUuid
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => U.UUID
  -> m Int64
deleteUserTokenByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result

deleteUserTokenByUuidAndTenantUuid
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => U.UUID
  -> U.UUID
  -> m Int64
deleteUserTokenByUuidAndTenantUuid uuid tenantUuid = do
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
