module Shared.Model.Workspace.Workspace where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Workspace = Workspace
  { uuid :: U.UUID
  , tenantUuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , defaultRoleUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
