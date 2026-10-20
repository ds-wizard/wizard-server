module Shared.Model.AuditLog.AuditLogSource where

import qualified Data.UUID as U
import GHC.Generics

data AuditLogSourceType
  = UserAuditLogSourceType
  | AutomationAuditLogSourceType
  | SystemAuditLogSourceType
  | McpAuditLogSourceType
  deriving (Show, Eq, Enum, Bounded, Generic)

data AuditLogSource = AuditLogSource
  { sType :: AuditLogSourceType
  , uuid :: Maybe U.UUID
  , clientName :: Maybe String
  , toolName :: Maybe String
  }
  deriving (Show, Eq, Generic)

userAuditLogSource :: AuditLogSource
userAuditLogSource = AuditLogSource {sType = UserAuditLogSourceType, uuid = Nothing, clientName = Nothing, toolName = Nothing}

systemAuditLogSource :: AuditLogSource
systemAuditLogSource = AuditLogSource {sType = SystemAuditLogSourceType, uuid = Nothing, clientName = Nothing, toolName = Nothing}

commandAuditLogSource :: U.UUID -> AuditLogSource
commandAuditLogSource commandUuid = AuditLogSource {sType = SystemAuditLogSourceType, uuid = Just commandUuid, clientName = Nothing, toolName = Nothing}

automationAuditLogSource :: U.UUID -> AuditLogSource
automationAuditLogSource automationUuid = AuditLogSource {sType = AutomationAuditLogSourceType, uuid = Just automationUuid, clientName = Nothing, toolName = Nothing}

mcpAuditLogSource :: Maybe U.UUID -> Maybe String -> String -> AuditLogSource
mcpAuditLogSource tokenUuid clientName toolName = AuditLogSource {sType = McpAuditLogSourceType, uuid = tokenUuid, clientName = clientName, toolName = Just toolName}

auditLogSourceTypeName :: AuditLogSourceType -> String
auditLogSourceTypeName UserAuditLogSourceType = "user"
auditLogSourceTypeName AutomationAuditLogSourceType = "automation"
auditLogSourceTypeName SystemAuditLogSourceType = "system"
auditLogSourceTypeName McpAuditLogSourceType = "mcp"

auditLogSourceTypeDescription :: AuditLogSourceType -> String
auditLogSourceTypeDescription UserAuditLogSourceType = "A request made by a signed-in user; the row's source.type."
auditLogSourceTypeDescription AutomationAuditLogSourceType = "A login automation; source.uuid is the automation."
auditLogSourceTypeDescription SystemAuditLogSourceType = "The system itself: a persistent command (source.uuid is the command) or a scheduled job (no uuid)."
auditLogSourceTypeDescription McpAuditLogSourceType = "A tool call of an MCP client; source.uuid is the token, source.clientName the client and source.toolName the tool."

parseAuditLogSourceType :: String -> Maybe AuditLogSourceType
parseAuditLogSourceType name = lookup name [(auditLogSourceTypeName t, t) | t <- [minBound .. maxBound]]
