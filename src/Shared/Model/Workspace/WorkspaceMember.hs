module Shared.Model.Workspace.WorkspaceMember where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data WorkspaceMember = WorkspaceMember
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
