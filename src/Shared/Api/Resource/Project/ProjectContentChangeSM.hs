module Shared.Api.Resource.Project.ProjectContentChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.Event.ProjectEventChangeSM ()
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Api.Resource.Project.ProjectContentChangeJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectContentChangeDTO where
  declareNamedSchema = toSwagger contentChangeDTO
