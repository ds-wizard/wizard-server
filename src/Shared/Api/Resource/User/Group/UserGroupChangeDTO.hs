module Shared.Api.Resource.User.Group.UserGroupChangeDTO where

import GHC.Generics

data UserGroupChangeDTO = UserGroupChangeDTO
  { name :: String
  , description :: Maybe String
  , private :: Bool
  }
  deriving (Generic)
