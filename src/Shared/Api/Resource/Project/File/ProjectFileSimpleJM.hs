module Shared.Api.Resource.Project.File.ProjectFileSimpleJM where

import Data.Aeson

import Shared.Model.Project.File.ProjectFileSimple
import Shared.Util.Aeson

instance FromJSON ProjectFileSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectFileSimple where
  toJSON = genericToJSON jsonOptions
