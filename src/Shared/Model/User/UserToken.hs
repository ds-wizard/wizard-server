module Shared.Model.User.UserToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenType
  = LoginUserTokenType
  | ApiKeyUserTokenType
  | McpUserTokenType
  deriving (Show, Eq, Generic, Read)

data UserToken = UserToken
  { uuid :: U.UUID
  , name :: String
  , tType :: UserTokenType
  , userUuid :: U.UUID
  , value :: String
  , userAgent :: String
  , sessionState :: Maybe String
  , expiresAt :: UTCTime
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserToken where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.tType == b.tType
      && a.userUuid == b.userUuid
      && a.value == b.value
      && a.userAgent == b.userAgent
      && a.sessionState == b.sessionState
      && a.expiresAt == b.expiresAt
      && a.tenantUuid == b.tenantUuid
