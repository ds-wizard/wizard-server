module Shared.Service.Project.ProjectTelemetry where

import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Service.Telemetry.TelemetryService

telemetryProjectListEvents :: WizardRequestContextC s m => U.UUID -> m ()
telemetryProjectListEvents projectUuid = logTelemetry "project" "listEvents" (U.toString projectUuid)

telemetryProjectDetailEvent :: WizardRequestContextC s m => U.UUID -> m ()
telemetryProjectDetailEvent projectUuid = logTelemetry "project" "detailEvent" (U.toString projectUuid)
