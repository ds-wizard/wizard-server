module Shared.Api.Resource.Workspace.WorkspaceChangeDTO where

import GHC.Generics

data WorkspaceChangeDTO = WorkspaceChangeDTO
  { name :: String
  , description :: Maybe String
  , primaryColor :: Maybe String
  }
  deriving (Show, Eq, Generic)
