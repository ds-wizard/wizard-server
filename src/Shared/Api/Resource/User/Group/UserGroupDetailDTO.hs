module Shared.Api.Resource.User.Group.UserGroupDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.User.UserWithMembershipDTO

data UserGroupDetailDTO = UserGroupDetailDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  , users :: [UserWithMembershipDTO]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
