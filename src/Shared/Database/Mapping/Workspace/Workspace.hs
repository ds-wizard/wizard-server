module Shared.Database.Mapping.Workspace.Workspace where

import Database.PostgreSQL.Simple

import Shared.Model.Workspace.Workspace

instance ToRow Workspace

instance FromRow Workspace
