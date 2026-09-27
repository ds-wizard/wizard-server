module Shared.Api.Resource.Workspace.WorkspaceMemberChangeSM where

import Data.Swagger

import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeJM ()
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Util.Swagger

instance ToSchema WorkspaceMemberChangeDTO where
  declareNamedSchema = toSwagger workspaceMemberChange
