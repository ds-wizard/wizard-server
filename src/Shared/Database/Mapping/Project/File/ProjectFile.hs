module Shared.Database.Mapping.Project.File.ProjectFile where

import Database.PostgreSQL.Simple

import Shared.Model.Project.File.ProjectFile

instance ToRow ProjectFile

instance FromRow ProjectFile
