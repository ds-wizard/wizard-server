module Shared.Api.Resource.User.UserDTO where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.User.UserSubmissionPropJM ()
import Shared.Model.User.RoleSimple
import Shared.Util.WizardHashable ()

data UserDTO = UserDTO
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
  , lastVisitedAt :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , emailVerifiedAt :: Maybe UTCTime
  , emailPending :: Maybe String
  }
  deriving (Show, Generic)

instance Eq UserDTO where
  a == b =
    a.uuid == b.uuid
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.email == b.email
      && a.affiliation == b.affiliation
      && a.role == b.role
      && a.active == b.active
      && a.imageUrl == b.imageUrl
      && a.locale == b.locale
      && a.lastSeenNewsId == b.lastSeenNewsId
      && a.emailVerifiedAt == b.emailVerifiedAt
      && a.emailPending == b.emailPending

instance Hashable UserDTO
