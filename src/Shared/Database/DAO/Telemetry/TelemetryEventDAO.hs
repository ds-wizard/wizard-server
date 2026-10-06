module Shared.Database.DAO.Telemetry.TelemetryEventDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Telemetry.TelemetryEvent ()
import Shared.Model.Context.RequestContext
import Shared.Model.Telemetry.TelemetryEvent

entityName = "telemetry_event"

findTelemetryEvents :: RequestContextC s sc m => m [TelemetryEvent]
findTelemetryEvents = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

insertTelemetryEvent :: RequestContextC s sc m => TelemetryEvent -> m Int64
insertTelemetryEvent = createInsertWithoutTransactionFn entityName

deleteTelemetryEvents :: RequestContextC s sc m => m Int64
deleteTelemetryEvents = createDeleteEntitiesFn entityName
