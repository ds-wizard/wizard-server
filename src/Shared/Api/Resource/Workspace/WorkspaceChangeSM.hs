module Shared.Api.Resource.Workspace.WorkspaceChangeSM where

import Data.Swagger

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Util.Swagger

instance ToSchema WorkspaceChangeDTO where
  declareNamedSchema = toSwagger workspaceChange
