module Shared.Api.Resource.Project.File.ProjectFileSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Util.Swagger

instance ToSchema ProjectFileSimple where
  declareNamedSchema = toSwagger projectFileSimple
