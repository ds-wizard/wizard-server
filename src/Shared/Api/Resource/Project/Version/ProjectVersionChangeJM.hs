module Shared.Api.Resource.Project.Version.ProjectVersionChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Util.Aeson

instance FromJSON ProjectVersionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionChangeDTO where
  toJSON = genericToJSON jsonOptions
