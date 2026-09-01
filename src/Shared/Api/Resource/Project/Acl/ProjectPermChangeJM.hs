module Shared.Api.Resource.Project.Acl.ProjectPermChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson

instance FromJSON ProjectPermChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPermChangeDTO where
  toJSON = genericToJSON jsonOptions
