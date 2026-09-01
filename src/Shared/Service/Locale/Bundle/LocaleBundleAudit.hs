module Shared.Service.Locale.Bundle.LocaleBundleAudit where

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Service.Audit.AuditService

auditLocaleBundleExport :: WizardRequestContextC s m => Coordinate -> m ()
auditLocaleBundleExport = logAudit "locale_bundle" "export" . show

auditLocaleBundlePullFromRegistry :: WizardRequestContextC s m => Coordinate -> m ()
auditLocaleBundlePullFromRegistry = logAudit "locale_bundle" "pullFromRegistry" . show

auditLocaleBundleImportFromFile :: WizardRequestContextC s m => Coordinate -> m ()
auditLocaleBundleImportFromFile = logAudit "locale_bundle" "importFromFile" . show
