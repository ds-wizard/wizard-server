module Shared.Model.User.UserTokenList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenList = UserTokenList
  { uuid :: U.UUID
  , name :: String
  , userAgent :: String
  , currentSession :: Bool
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserTokenList where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.userAgent == b.userAgent
      && a.currentSession == b.currentSession
      && a.expiresAt == b.expiresAt
