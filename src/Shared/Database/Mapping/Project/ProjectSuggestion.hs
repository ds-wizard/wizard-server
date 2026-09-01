module Shared.Database.Mapping.Project.ProjectSuggestion where

import Database.PostgreSQL.Simple

import Shared.Model.Project.ProjectSuggestion

instance FromRow ProjectSuggestion
