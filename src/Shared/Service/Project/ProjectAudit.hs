module Shared.Service.Project.ProjectAudit where

import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Service.Audit.AuditService

auditProjectListEvents :: WizardRequestContextC s m => U.UUID -> m ()
auditProjectListEvents projectUuid = logAudit "project" "listEvents" (U.toString projectUuid)

auditProjectDetailEvent :: WizardRequestContextC s m => U.UUID -> m ()
auditProjectDetailEvent projectUuid = logAudit "project" "detailEvent" (U.toString projectUuid)
