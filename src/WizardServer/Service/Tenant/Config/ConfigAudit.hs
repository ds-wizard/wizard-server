module WizardServer.Service.Tenant.Config.ConfigAudit where

import Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Service.Audit.AuditService

auditChangeColors :: WizardRequestContextC s m => U.UUID -> m ()
auditChangeColors aUuid = logAudit "tenant_config" "changeColors" (U.toString aUuid)
