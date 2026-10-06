module Shared.Database.Mapping.Telemetry.TelemetryEvent where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Telemetry.TelemetryEvent

instance FromRow TelemetryEvent

instance ToRow TelemetryEvent
