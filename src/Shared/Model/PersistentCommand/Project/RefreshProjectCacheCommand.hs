module Shared.Model.PersistentCommand.Project.RefreshProjectCacheCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data RefreshProjectCacheCommand = RefreshProjectCacheCommand
  { tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON RefreshProjectCacheCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RefreshProjectCacheCommand where
  toJSON = genericToJSON jsonOptions
