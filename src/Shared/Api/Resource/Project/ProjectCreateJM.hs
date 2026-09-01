module Shared.Api.Resource.Project.ProjectCreateJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson

instance FromJSON ProjectCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCreateDTO where
  toJSON = genericToJSON jsonOptions
