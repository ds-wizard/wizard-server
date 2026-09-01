module Shared.Api.Resource.Project.File.ProjectFileListSM where

import Data.Swagger

import Shared.Api.Resource.Project.File.ProjectFileListJM ()
import Shared.Api.Resource.Project.ProjectSimpleSM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Model.Project.File.ProjectFileList
import Shared.Util.Swagger

instance ToSchema ProjectFileList where
  declareNamedSchema = toSwagger projectFileList
