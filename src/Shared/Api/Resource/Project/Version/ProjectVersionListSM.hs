module Shared.Api.Resource.Project.Version.ProjectVersionListSM where

import Data.Swagger

import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Util.Swagger

instance ToSchema ProjectVersionList where
  declareNamedSchema = toSwagger (projectVersion1List project1Uuid)
