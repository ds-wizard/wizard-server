module Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit where

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Service.Audit.AuditService

auditBundleExport :: WizardRequestContextC s m => Coordinate -> m ()
auditBundleExport = logAudit "document_template_bundle" "export" . show

auditBundlePullFromRegistry :: WizardRequestContextC s m => Coordinate -> m ()
auditBundlePullFromRegistry = logAudit "document_template_bundle" "pullFromRegistry" . show

auditBundleImportFromFile :: WizardRequestContextC s m => Coordinate -> m ()
auditBundleImportFromFile = logAudit "document_template_bundle" "importFromFile" . show
