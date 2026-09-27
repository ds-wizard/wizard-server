module Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data WorkspaceMemberChangeDTO = WorkspaceMemberChangeDTO
  { roleUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
