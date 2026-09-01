module Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAudit where

import Shared.Model.Context.WizardRequestContext
import Shared.Service.Audit.AuditService

auditKnowledgeModelBundleExport :: WizardRequestContextC s m => String -> m ()
auditKnowledgeModelBundleExport = logAudit "knowledge_model_bundle" "export"

auditKnowledgeModelBundlePullFromRegistry :: WizardRequestContextC s m => String -> m ()
auditKnowledgeModelBundlePullFromRegistry = logAudit "knowledge_model_bundle" "pullFromRegistry"

auditKnowledgeModelBundleImportFromFile :: WizardRequestContextC s m => String -> m ()
auditKnowledgeModelBundleImportFromFile = logAudit "knowledge_model_bundle" "importFromFile"
