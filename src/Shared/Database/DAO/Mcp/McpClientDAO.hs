module Shared.Database.DAO.Mcp.McpClientDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Mcp.McpClient ()
import Shared.Model.Context.RequestContext
import Shared.Model.Mcp.McpClient

entityName = "mcp_client"

findMcpClientByClientId :: RequestContextC s sc m => String -> m McpClient
findMcpClientByClientId clientId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("client_id", clientId)]

findMcpClientByClientId' :: RequestContextC s sc m => String -> m (Maybe McpClient)
findMcpClientByClientId' clientId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("client_id", clientId)]

insertMcpClient :: RequestContextC s sc m => McpClient -> m Int64
insertMcpClient = createInsertFn entityName

deleteMcpClients :: RequestContextC s sc m => m Int64
deleteMcpClients = createDeleteEntitiesFn entityName
