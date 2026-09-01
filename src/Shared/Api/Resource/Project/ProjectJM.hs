module Shared.Api.Resource.Project.ProjectJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectReportJM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectStateJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.User.UserJM ()
import Shared.Util.Aeson

instance FromJSON ProjectDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDTO where
  toJSON = genericToJSON jsonOptions
