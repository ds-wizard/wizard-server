module Shared.Model.Workspace.WorkspaceMembership where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data WorkspaceMembership = WorkspaceMembership
  { workspaceUuid :: U.UUID
  , userUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , roleUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
