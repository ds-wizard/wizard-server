module Shared.Service.Telemetry.TelemetryMapper where

import Shared.Model.Telemetry.TelemetryEvent

toTelemetryEvent uuid component action entity body createdBy tenantUuid createdAt =
  TelemetryEvent
    { uuid = uuid
    , component = component
    , action = action
    , entity = entity
    , body = body
    , createdBy = createdBy
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    }
