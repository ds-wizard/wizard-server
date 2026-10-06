module Shared.Database.Mapping.AuditLog.AuditLog where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common ()
import Shared.Model.AuditLog.AuditLog
import Shared.Model.AuditLog.AuditLogSource

instance ToField AuditLogSourceType where
  toField = toField . auditLogSourceTypeName

instance FromField AuditLogSourceType where
  fromField f dat = do
    name <- fromField f dat
    maybe (returnError ConversionFailed f name) return (parseAuditLogSourceType name)

instance ToRow AuditLog

instance FromRow AuditLog
