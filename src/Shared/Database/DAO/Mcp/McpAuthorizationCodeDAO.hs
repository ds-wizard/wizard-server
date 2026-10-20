module Shared.Database.DAO.Mcp.McpAuthorizationCodeDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Mcp.McpAuthorizationCode ()
import Shared.Model.Context.RequestContext
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Util.Logger
import Shared.Util.String

entityName = "mcp_authorization_code"

findMcpAuthorizationCodeByCode :: RequestContextC s sc m => String -> m McpAuthorizationCode
findMcpAuthorizationCodeByCode code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("code", code)]

findMcpAuthorizationCodeByCode' :: RequestContextC s sc m => String -> m (Maybe McpAuthorizationCode)
findMcpAuthorizationCodeByCode' code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("code", code)]

insertMcpAuthorizationCode :: RequestContextC s sc m => McpAuthorizationCode -> m Int64
insertMcpAuthorizationCode = createInsertFn entityName

deleteMcpAuthorizationCodeByCode :: RequestContextC s sc m => String -> m Int64
deleteMcpAuthorizationCodeByCode code = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("code", code)]

deleteMcpAuthorizationCodes :: RequestContextC s sc m => m Int64
deleteMcpAuthorizationCodes = createDeleteEntitiesFn entityName

deleteMcpAuthorizationCodesWithExpiration :: RequestContextC s sc m => m Int64
deleteMcpAuthorizationCodesWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
