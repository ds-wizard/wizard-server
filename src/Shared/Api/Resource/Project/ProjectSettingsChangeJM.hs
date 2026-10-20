module Shared.Api.Resource.Project.ProjectSettingsChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON ProjectSettingsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSettingsChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectSettingsChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
