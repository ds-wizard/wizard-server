module Shared.Model.UserEmailLink.UserEmailLink where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserEmailLink identity aType = UserEmailLink
  { uuid :: U.UUID
  , identity :: identity
  , aType :: aType
  , hash :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance (Eq identity, Eq aType) => Eq (UserEmailLink identity aType) where
  a == b =
    a.uuid == b.uuid
      && a.identity == b.identity
      && a.aType == b.aType
      && a.hash == b.hash
      && a.tenantUuid == b.tenantUuid
