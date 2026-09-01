module Shared.Database.Mapping.Project.File.ProjectFileSimple where

import Database.PostgreSQL.Simple

import Shared.Model.Project.File.ProjectFileSimple

instance FromRow ProjectFileSimple
