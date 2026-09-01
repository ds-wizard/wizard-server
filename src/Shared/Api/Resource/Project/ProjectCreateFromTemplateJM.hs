module Shared.Api.Resource.Project.ProjectCreateFromTemplateJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Shared.Util.Aeson

instance FromJSON ProjectCreateFromTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCreateFromTemplateDTO where
  toJSON = genericToJSON jsonOptions
