module Shared.Database.DAO.User.UserOpenIdIdentityDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.UserOpenIdIdentity ()
import Shared.Database.Mapping.User.UserOpenIdIdentityList ()
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserOpenIdIdentity
import Shared.Model.User.UserOpenIdIdentityList
import Shared.Util.String

entityName = "user_openid_identity"

findUserOpenIdIdentitiesByUserUuid :: RequestContextC s sc m => U.UUID -> m [UserOpenIdIdentity]
findUserOpenIdIdentitiesByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserOpenIdIdentityListsByUserUuid :: RequestContextC s sc m => U.UUID -> m [UserOpenIdIdentityList]
findUserOpenIdIdentityListsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let clientTable = "openid_client"
  let sql =
        fromString $
          f'
            "SELECT i.uuid, i.external_id, i.external_label, i.provider_uuid, oc.name, oc.style, i.created_at \
            \FROM %s i \
            \JOIN %s oc ON oc.uuid = i.provider_uuid \
            \WHERE i.user_uuid = ? AND i.tenant_uuid = ?"
            [entityName, clientTable]
  let params = [toField userUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUserOpenIdIdentityByExternalIdAndProvider' :: RequestContextC s sc m => String -> U.UUID -> m (Maybe UserOpenIdIdentity)
findUserOpenIdIdentityByExternalIdAndProvider' externalId providerUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn'
    entityName
    [tenantQueryUuid tenantUuid, ("external_id", externalId), ("provider_uuid", U.toString providerUuid)]

insertUserOpenIdIdentity :: RequestContextC s sc m => UserOpenIdIdentity -> m Int64
insertUserOpenIdIdentity identity = do
  createInsertFn entityName identity

deleteUserOpenIdIdentities :: RequestContextC s sc m => m Int64
deleteUserOpenIdIdentities = do
  createDeleteEntitiesFn entityName

deleteUserOpenIdIdentityByUuid :: RequestContextC s sc m => U.UUID -> m ()
deleteUserOpenIdIdentityByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
