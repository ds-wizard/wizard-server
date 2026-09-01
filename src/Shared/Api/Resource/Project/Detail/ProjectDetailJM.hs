module Shared.Api.Resource.Project.Detail.ProjectDetailJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Util.Aeson

instance FromJSON ProjectDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailDTO where
  toJSON = genericToJSON jsonOptions
