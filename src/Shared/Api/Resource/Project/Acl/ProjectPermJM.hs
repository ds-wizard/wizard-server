module Shared.Api.Resource.Project.Acl.ProjectPermJM where

import Data.Aeson

import Shared.Api.Resource.Acl.MemberJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Util.Aeson

instance FromJSON ProjectPermType

instance ToJSON ProjectPermType

instance FromJSON ProjectPerm where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPerm where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectPermDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPermDTO where
  toJSON = genericToJSON jsonOptions
