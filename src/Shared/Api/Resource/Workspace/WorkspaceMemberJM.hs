module Shared.Api.Resource.Workspace.WorkspaceMemberJM where

import Data.Aeson

import Shared.Api.Resource.User.RoleSimpleJM ()
import Shared.Model.Workspace.WorkspaceMember
import Shared.Util.Aeson

instance FromJSON WorkspaceMember where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WorkspaceMember where
  toJSON = genericToJSON jsonOptions
