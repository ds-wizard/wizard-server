module Shared.Database.Mapping.Project.Cache.ProjectCacheSource where

import Database.PostgreSQL.Simple

import Shared.Model.Project.Cache.ProjectCacheSource

instance FromRow ProjectCacheSource
