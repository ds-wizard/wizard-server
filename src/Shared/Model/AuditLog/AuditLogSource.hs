module Shared.Model.AuditLog.AuditLogSource where

import qualified Data.UUID as U
import GHC.Generics

data AuditLogSourceType
  = UserAuditLogSourceType
  | AutomationAuditLogSourceType
  | SystemAuditLogSourceType
  deriving (Show, Eq, Enum, Bounded, Generic)

data AuditLogSource = AuditLogSource
  { sType :: AuditLogSourceType
  , uuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

userAuditLogSource :: AuditLogSource
userAuditLogSource = AuditLogSource {sType = UserAuditLogSourceType, uuid = Nothing}

systemAuditLogSource :: AuditLogSource
systemAuditLogSource = AuditLogSource {sType = SystemAuditLogSourceType, uuid = Nothing}

commandAuditLogSource :: U.UUID -> AuditLogSource
commandAuditLogSource commandUuid = AuditLogSource {sType = SystemAuditLogSourceType, uuid = Just commandUuid}

automationAuditLogSource :: U.UUID -> AuditLogSource
automationAuditLogSource automationUuid = AuditLogSource {sType = AutomationAuditLogSourceType, uuid = Just automationUuid}

auditLogSourceTypeName :: AuditLogSourceType -> String
auditLogSourceTypeName UserAuditLogSourceType = "user"
auditLogSourceTypeName AutomationAuditLogSourceType = "automation"
auditLogSourceTypeName SystemAuditLogSourceType = "system"

auditLogSourceTypeDescription :: AuditLogSourceType -> String
auditLogSourceTypeDescription UserAuditLogSourceType = "A request made by a signed-in user; the row's source.type."
auditLogSourceTypeDescription AutomationAuditLogSourceType = "A login automation; source.uuid is the automation."
auditLogSourceTypeDescription SystemAuditLogSourceType = "The system itself: a persistent command (source.uuid is the command) or a scheduled job (no uuid)."

parseAuditLogSourceType :: String -> Maybe AuditLogSourceType
parseAuditLogSourceType name = lookup name [(auditLogSourceTypeName t, t) | t <- [minBound .. maxBound]]
