module Shared.Model.AuditLog.AuditLogScope where

import GHC.Generics

data AuditLogScope
  = OrganizationAuditLogScope
  | WorkspaceAuditLogScope
  | OrganizationOrWorkspaceAuditLogScope
  deriving (Show, Eq, Enum, Bounded, Generic)

auditLogScopeName :: AuditLogScope -> String
auditLogScopeName OrganizationAuditLogScope = "organization"
auditLogScopeName WorkspaceAuditLogScope = "workspace"
auditLogScopeName OrganizationOrWorkspaceAuditLogScope = "organizationOrWorkspace"

auditLogScopeDescription :: AuditLogScope -> String
auditLogScopeDescription OrganizationAuditLogScope = "organization: the workspace of the row is always null."
auditLogScopeDescription WorkspaceAuditLogScope = "workspace: the workspace of the row is always set."
auditLogScopeDescription OrganizationOrWorkspaceAuditLogScope = "organizationOrWorkspace: the plane of the entity, so the workspace of the row is set only for a workspace entity."
