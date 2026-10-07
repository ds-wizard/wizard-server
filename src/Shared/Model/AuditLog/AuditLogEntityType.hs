module Shared.Model.AuditLog.AuditLogEntityType where

import GHC.Generics

data AuditLogEntityType
  = ProjectAuditLogEntityType
  | DocumentAuditLogEntityType
  | ProjectFileAuditLogEntityType
  | WorkspaceAuditLogEntityType
  | UserAuditLogEntityType
  | ApiKeyAuditLogEntityType
  | RoleAuditLogEntityType
  | UserGroupAuditLogEntityType
  | KnowledgeModelAuditLogEntityType
  | KnowledgeModelEditorAuditLogEntityType
  | KnowledgeModelSecretAuditLogEntityType
  | DocumentTemplateAuditLogEntityType
  | DocumentTemplateEditorAuditLogEntityType
  | LocaleAuditLogEntityType
  | MailTemplateLocaleAuditLogEntityType
  | DataSourceAuditLogEntityType
  | PluginAuditLogEntityType
  | AutomationAuditLogEntityType
  | OpenIdClientAuditLogEntityType
  | SamlServiceProviderAuditLogEntityType
  | TenantPlanAuditLogEntityType
  | AnalyticsViewAuditLogEntityType
  | OrganizationAuditLogEntityType
  deriving (Show, Eq, Enum, Bounded, Generic)

auditLogEntityTypeName :: AuditLogEntityType -> String
auditLogEntityTypeName ProjectAuditLogEntityType = "project"
auditLogEntityTypeName DocumentAuditLogEntityType = "document"
auditLogEntityTypeName ProjectFileAuditLogEntityType = "projectFile"
auditLogEntityTypeName WorkspaceAuditLogEntityType = "workspace"
auditLogEntityTypeName UserAuditLogEntityType = "user"
auditLogEntityTypeName ApiKeyAuditLogEntityType = "apiKey"
auditLogEntityTypeName RoleAuditLogEntityType = "role"
auditLogEntityTypeName UserGroupAuditLogEntityType = "userGroup"
auditLogEntityTypeName KnowledgeModelAuditLogEntityType = "knowledgeModel"
auditLogEntityTypeName KnowledgeModelEditorAuditLogEntityType = "knowledgeModelEditor"
auditLogEntityTypeName KnowledgeModelSecretAuditLogEntityType = "knowledgeModelSecret"
auditLogEntityTypeName DocumentTemplateAuditLogEntityType = "documentTemplate"
auditLogEntityTypeName DocumentTemplateEditorAuditLogEntityType = "documentTemplateEditor"
auditLogEntityTypeName LocaleAuditLogEntityType = "locale"
auditLogEntityTypeName MailTemplateLocaleAuditLogEntityType = "mailTemplateLocale"
auditLogEntityTypeName DataSourceAuditLogEntityType = "dataSource"
auditLogEntityTypeName PluginAuditLogEntityType = "plugin"
auditLogEntityTypeName AutomationAuditLogEntityType = "automation"
auditLogEntityTypeName OpenIdClientAuditLogEntityType = "openIdClient"
auditLogEntityTypeName SamlServiceProviderAuditLogEntityType = "samlServiceProvider"
auditLogEntityTypeName TenantPlanAuditLogEntityType = "tenantPlan"
auditLogEntityTypeName AnalyticsViewAuditLogEntityType = "analyticsView"
auditLogEntityTypeName OrganizationAuditLogEntityType = "organization"

auditLogEntityTypeDescription :: AuditLogEntityType -> String
auditLogEntityTypeDescription ProjectAuditLogEntityType = "A project; entity.uuid is the project uuid."
auditLogEntityTypeDescription DocumentAuditLogEntityType = "A document; entity.uuid is the document uuid."
auditLogEntityTypeDescription ProjectFileAuditLogEntityType = "A project file; entity.uuid is the file uuid."
auditLogEntityTypeDescription WorkspaceAuditLogEntityType = "A workspace; entity.uuid is the workspace uuid."
auditLogEntityTypeDescription UserAuditLogEntityType = "A user; entity.uuid is the user uuid and entity.name the email."
auditLogEntityTypeDescription ApiKeyAuditLogEntityType = "An API key; entity.uuid is the token uuid."
auditLogEntityTypeDescription RoleAuditLogEntityType = "A role; entity.uuid is the role uuid."
auditLogEntityTypeDescription UserGroupAuditLogEntityType = "A user group; entity.uuid is the group uuid."
auditLogEntityTypeDescription KnowledgeModelAuditLogEntityType = "A knowledge model package; entity.uuid is the package uuid."
auditLogEntityTypeDescription KnowledgeModelEditorAuditLogEntityType = "A knowledge model editor; entity.uuid is the editor uuid."
auditLogEntityTypeDescription KnowledgeModelSecretAuditLogEntityType = "A knowledge model secret; entity.uuid is the secret uuid."
auditLogEntityTypeDescription DocumentTemplateAuditLogEntityType = "A document template; entity.uuid is the template uuid."
auditLogEntityTypeDescription DocumentTemplateEditorAuditLogEntityType = "A document template editor; entity.uuid is the draft template uuid."
auditLogEntityTypeDescription LocaleAuditLogEntityType = "A locale; entity.uuid is the locale uuid."
auditLogEntityTypeDescription MailTemplateLocaleAuditLogEntityType = "A mail template locale; entity.uuid is the mail template locale uuid."
auditLogEntityTypeDescription DataSourceAuditLogEntityType = "A data source; entity.uuid is the data source uuid."
auditLogEntityTypeDescription PluginAuditLogEntityType = "A plugin; entity.uuid is the plugin uuid."
auditLogEntityTypeDescription AutomationAuditLogEntityType = "An automation; entity.uuid is the automation uuid."
auditLogEntityTypeDescription OpenIdClientAuditLogEntityType = "An OpenID client; entity.uuid is the client uuid."
auditLogEntityTypeDescription SamlServiceProviderAuditLogEntityType = "A SAML service provider; entity.uuid is the provider uuid."
auditLogEntityTypeDescription TenantPlanAuditLogEntityType = "A tenant plan; entity.uuid is the plan uuid."
auditLogEntityTypeDescription AnalyticsViewAuditLogEntityType = "An analytics view; entity.uuid is the view uuid."
auditLogEntityTypeDescription OrganizationAuditLogEntityType = "The organization (tenant) itself; entity.uuid is the tenant uuid."
