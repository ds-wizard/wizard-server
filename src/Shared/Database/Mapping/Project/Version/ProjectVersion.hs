module Shared.Database.Mapping.Project.Version.ProjectVersion where

import Database.PostgreSQL.Simple

import Shared.Model.Project.Version.ProjectVersion

instance ToRow ProjectVersion

instance FromRow ProjectVersion
