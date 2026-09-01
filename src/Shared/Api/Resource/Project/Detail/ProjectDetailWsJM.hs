module Shared.Api.Resource.Project.Detail.ProjectDetailWsJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON ProjectDetailWsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailWsDTO where
  toJSON = genericToJSON jsonOptions
