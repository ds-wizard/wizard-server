module Shared.Database.Mapping.TemporaryFile.TemporaryFile where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Common ()
import Shared.Model.TemporaryFile.TemporaryFile

instance FromRow TemporaryFile

instance ToRow TemporaryFile
