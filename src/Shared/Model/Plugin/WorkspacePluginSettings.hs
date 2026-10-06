module Shared.Model.Plugin.WorkspacePluginSettings where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data WorkspacePluginSettings = WorkspacePluginSettings
  { workspaceUuid :: U.UUID
  , pluginUuid :: U.UUID
  , tenantUuid :: U.UUID
  , enabled :: Bool
  , values :: Maybe A.Value
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
