module Shared.Api.Resource.Project.File.ProjectFileListJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectSimpleJM ()
import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.File.ProjectFileList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON ProjectFileList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectFileList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectFileList where
  jsonSchema = genericJsonSchema jsonOptions
