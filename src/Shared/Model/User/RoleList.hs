module Shared.Model.User.RoleList where

import qualified Data.UUID as U
import GHC.Generics

data RoleList = RoleList
  { uuid :: U.UUID
  , name :: String
  , permissions :: [String]
  , usersCount :: Int
  , isAdmin :: Bool
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
