module Shared.Api.Resource.Project.ProjectSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectSuggestionJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.ProjectSuggestion
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectSuggestion where
  declareNamedSchema = toSwagger (toSuggestion project1)
