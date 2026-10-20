module Shared.Database.DAO.Mcp.McpRefreshTokenDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Mcp.McpRefreshToken ()
import Shared.Model.Context.RequestContext
import Shared.Model.Mcp.McpRefreshToken
import Shared.Util.Logger
import Shared.Util.String

entityName = "mcp_refresh_token"

findMcpRefreshTokenByTokenHash' :: RequestContextC s sc m => String -> m (Maybe McpRefreshToken)
findMcpRefreshTokenByTokenHash' tokenHash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("token_hash", tokenHash)]

findMcpRefreshTokenByPreviousTokenHash' :: RequestContextC s sc m => String -> m (Maybe McpRefreshToken)
findMcpRefreshTokenByPreviousTokenHash' previousTokenHash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("previous_token_hash", previousTokenHash)]

findMcpRefreshTokenByUserTokenUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe McpRefreshToken)
findMcpRefreshTokenByUserTokenUuid' userTokenUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("user_token_uuid", U.toString userTokenUuid)]

insertMcpRefreshToken :: RequestContextC s sc m => McpRefreshToken -> m Int64
insertMcpRefreshToken = createInsertFn entityName

deleteMcpRefreshTokenByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteMcpRefreshTokenByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deleteMcpRefreshTokensWithExpiration :: RequestContextC s sc m => m Int64
deleteMcpRefreshTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteMcpRefreshTokens :: RequestContextC s sc m => m Int64
deleteMcpRefreshTokens = createDeleteEntitiesFn entityName
