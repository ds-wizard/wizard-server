module Shared.Model.AuditLog.AuditLogEventDefinition.Internal where

import Shared.Model.AuditLog.AuditLogEntityType
import Shared.Model.AuditLog.AuditLogScope

data AuditLogEventDefinition c d = AuditLogEventDefinition
  { component :: String
  , action :: String
  , description :: String
  , scope :: AuditLogScope
  , eventBridge :: Bool
  , entityType :: AuditLogEntityType
  , entityDescription :: String
  , example :: d
  , changesExample :: Maybe c
  }

auditLogEvent
  :: String
  -> String
  -> String
  -> AuditLogScope
  -> Bool
  -> AuditLogEntityType
  -> String
  -> d
  -> AuditLogEventDefinition c d
auditLogEvent component action description scope eventBridge entityType entityDescription example =
  AuditLogEventDefinition {changesExample = Nothing, ..}

withChangesExample :: c -> AuditLogEventDefinition c d -> AuditLogEventDefinition c d
withChangesExample changes definition = definition {changesExample = Just changes}
