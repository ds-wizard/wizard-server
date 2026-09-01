module Shared.Api.Resource.Project.ProjectShareChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.Acl.ProjectPermChangeJM ()
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson

instance FromJSON ProjectShareChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectShareChangeDTO where
  toJSON = genericToJSON jsonOptions
