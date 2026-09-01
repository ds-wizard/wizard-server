module Shared.Api.Resource.Project.Version.ProjectVersionRevertJM where

import Data.Aeson

import Shared.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Shared.Util.Aeson

instance FromJSON ProjectVersionRevertDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionRevertDTO where
  toJSON = genericToJSON jsonOptions
