module Shared.Api.Resource.Project.ProjectSettingsChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Util.Aeson

instance FromJSON ProjectSettingsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSettingsChangeDTO where
  toJSON = genericToJSON jsonOptions
