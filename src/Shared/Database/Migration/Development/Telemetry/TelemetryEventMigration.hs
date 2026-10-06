module Shared.Database.Migration.Development.Telemetry.TelemetryEventMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Telemetry.TelemetryEventDAO
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Telemetry/TelemetryEvent) started"
  deleteTelemetryEvents
  logInfo _CMP_MIGRATION "(Telemetry/TelemetryEvent) ended"
