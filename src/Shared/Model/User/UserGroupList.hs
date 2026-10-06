module Shared.Model.User.UserGroupList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserGroupList = UserGroupList
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  , editable :: Bool
  , member :: Bool
  , updatedAt :: UTCTime
  , workspaceUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)
