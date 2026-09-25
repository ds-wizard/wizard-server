module Shared.Database.Mapping.Workspace.WorkspaceMember where

import Database.PostgreSQL.Simple

import Shared.Model.Workspace.WorkspaceMember

instance FromRow WorkspaceMember
