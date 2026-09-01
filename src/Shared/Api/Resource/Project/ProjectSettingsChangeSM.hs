module Shared.Api.Resource.Project.ProjectSettingsChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Api.Resource.Project.ProjectSettingsChangeJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectSettingsChangeDTO where
  declareNamedSchema = toSwagger project1SettingsChange
