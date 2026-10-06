module Shared.Api.Resource.Workspace.WorkspaceJM where

import Data.Aeson

import Shared.Model.Workspace.Workspace
import Shared.Util.Aeson

instance FromJSON Workspace where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Workspace where
  toJSON = genericToJSON jsonOptions
