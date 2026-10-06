module Shared.Model.Plugin.Plugin where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Plugin = Plugin
  { uuid :: U.UUID
  , url :: String
  , enabled :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , workspaceOverrideAllowed :: Bool
  }
  deriving (Generic, Eq, Show)
