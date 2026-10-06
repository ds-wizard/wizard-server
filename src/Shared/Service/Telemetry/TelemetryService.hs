module Shared.Service.Telemetry.TelemetryService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Telemetry.TelemetryEventDAO
import Shared.Model.Context.RequestContext
import Shared.Service.Telemetry.TelemetryMapper
import Shared.Util.Uuid

logTelemetry :: RequestContextC s sc m => String -> String -> String -> m ()
logTelemetry component action entity = logTelemetryWithBody component action entity M.empty

logTelemetryWithBody :: RequestContextC s sc m => String -> String -> String -> M.Map String String -> m ()
logTelemetryWithBody component action entity body = do
  tUuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  createdBy <- asks (.identity')
  now <- liftIO getCurrentTime
  let telemetryEvent = toTelemetryEvent tUuid component action entity body (fmap (fromJust . U.fromString) createdBy) tenantUuid now
  insertTelemetryEvent telemetryEvent
  return ()
