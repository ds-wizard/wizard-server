module Shared.Model.Workspace.WorkspaceMember where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.User.RoleSimple

data WorkspaceMember = WorkspaceMember
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  , createdAt :: UTCTime
  , role :: RoleSimple
  }
  deriving (Show, Eq, Generic)
