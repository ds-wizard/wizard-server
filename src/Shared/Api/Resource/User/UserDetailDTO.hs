module Shared.Api.Resource.User.UserDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Model.User.RoleSimple

data UserDetailDTO = UserDetailDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , role :: RoleSimple
  , active :: Bool
  , imageUrl :: Maybe String
  , locale :: Maybe U.UUID
  , lastSeenNewsId :: Maybe String
  , emailVerifiedAt :: Maybe UTCTime
  , emailPending :: Maybe String
  , externalIdentities :: [UserOpenIdIdentityDTO]
  , lastVisitedAt :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)
