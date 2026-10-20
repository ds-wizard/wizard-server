module Shared.Database.DAO.Mcp.McpUsedConfirmationTokenDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger
import Shared.Util.String

entityName = "mcp_used_confirmation_token"

insertMcpUsedConfirmationToken :: RequestContextC s sc m => String -> UTCTime -> m Int64
insertMcpUsedConfirmationToken tokenHash expiresAt = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "INSERT INTO %s VALUES (?, ?, ?, now()) ON CONFLICT (token_hash) DO NOTHING" [entityName]
  let params = [toField tokenHash, toField tenantUuid, toField expiresAt]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteMcpUsedConfirmationTokensWithExpiration :: RequestContextC s sc m => m Int64
deleteMcpUsedConfirmationTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
