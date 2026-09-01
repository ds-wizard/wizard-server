module Shared.Database.Mapping.Audit.Audit where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.Audit.Audit

instance FromRow Audit

instance ToRow Audit
