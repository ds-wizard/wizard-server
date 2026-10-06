module Shared.Api.Resource.Workspace.WorkspaceSM where

import Data.Swagger

import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Workspace.Workspace
import Shared.Util.Swagger

instance ToSchema Workspace where
  declareNamedSchema = toSwagger defaultWorkspace
