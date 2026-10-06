module Shared.Model.AuditLog.AuditLogEventDefinition (
  AuditLogEventDefinition,
  SomeAuditLogEventDefinition (..),
  definitionComponent,
  definitionAction,
  definitionDescription,
  definitionScope,
  definitionEventBridge,
  definitionEntityType,
  definitionEntityDescription,
  definitionExample,
  definitionChangesExample,
  someDefinitionKey,
) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)

import Shared.Model.AuditLog.AuditLogEntityType
import Shared.Model.AuditLog.AuditLogEventDefinition.Internal
import Shared.Model.AuditLog.AuditLogScope

data SomeAuditLogEventDefinition
  = forall c d. (ToJSON c, ToSchema c, ToJSON d, ToSchema d) => SomeAuditLogEventDefinition (AuditLogEventDefinition c d)

definitionComponent :: AuditLogEventDefinition c d -> String
definitionComponent definition = definition.component

definitionAction :: AuditLogEventDefinition c d -> String
definitionAction definition = definition.action

definitionDescription :: AuditLogEventDefinition c d -> String
definitionDescription definition = definition.description

definitionScope :: AuditLogEventDefinition c d -> AuditLogScope
definitionScope definition = definition.scope

definitionEventBridge :: AuditLogEventDefinition c d -> Bool
definitionEventBridge definition = definition.eventBridge

definitionEntityType :: AuditLogEventDefinition c d -> AuditLogEntityType
definitionEntityType definition = definition.entityType

definitionEntityDescription :: AuditLogEventDefinition c d -> String
definitionEntityDescription definition = definition.entityDescription

definitionExample :: AuditLogEventDefinition c d -> d
definitionExample definition = definition.example

definitionChangesExample :: AuditLogEventDefinition c d -> Maybe c
definitionChangesExample definition = definition.changesExample

someDefinitionKey :: SomeAuditLogEventDefinition -> (String, String)
someDefinitionKey (SomeAuditLogEventDefinition definition) = (definition.component, definition.action)
