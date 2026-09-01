module Shared.Model.PersistentCommand.PersistentCommandList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.Tenant.TenantSuggestion
import Shared.Model.User.UserSuggestion

data PersistentCommandList = PersistentCommandList
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , attempts :: Int
  , maxAttempts :: Int
  , tenant :: TenantSuggestion
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
