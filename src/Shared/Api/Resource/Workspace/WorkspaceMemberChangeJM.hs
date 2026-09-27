module Shared.Api.Resource.Workspace.WorkspaceMemberChangeJM where

import Data.Aeson

import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Util.Aeson

instance FromJSON WorkspaceMemberChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WorkspaceMemberChangeDTO where
  toJSON = genericToJSON jsonOptions
