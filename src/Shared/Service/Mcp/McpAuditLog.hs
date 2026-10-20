module Shared.Service.Mcp.McpAuditLog where

import qualified Data.UUID as U

import Shared.Database.DAO.User.UserDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.UserAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpClient
import Shared.Model.User.User
import Shared.Model.User.UserToken
import Shared.Service.AuditLog.AuditLogMapper
import Shared.Service.AuditLog.AuditLogService

al_connectMcp :: WizardRequestContextC s m => User -> McpClient -> U.UUID -> m ()
al_connectMcp user client tokenUuid =
  asUser user $
    logAuditLog mcpConnect (entityOf user.uuid user.email) AuditLogMcpConnectData {tokenUuid = tokenUuid, clientName = client.clientName}

al_deleteMcpToken :: WizardRequestContextC s m => UserToken -> m ()
al_deleteMcpToken userToken = do
  user <- findUserByUuid userToken.userUuid
  logAuditLog mcpTokenDelete (entityOf user.uuid user.email) (toSessionData userToken)
