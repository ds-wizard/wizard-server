module Shared.Api.Resource.Workspace.WorkspaceChangeJM where

import Data.Aeson

import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON WorkspaceChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WorkspaceChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema WorkspaceChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
