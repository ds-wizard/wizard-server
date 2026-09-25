module Shared.Database.Mapping.Workspace.WorkspaceMembership where

import Database.PostgreSQL.Simple

import Shared.Model.Workspace.WorkspaceMembership

instance ToRow WorkspaceMembership

instance FromRow WorkspaceMembership
