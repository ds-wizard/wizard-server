module Shared.Model.User.UserWithMembership where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.User.UserGroupMembership

data UserWithMembership = UserWithMembership
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  , affiliation :: Maybe String
  , membershipType :: UserGroupMembershipType
  }
  deriving (Generic, Eq, Show)
