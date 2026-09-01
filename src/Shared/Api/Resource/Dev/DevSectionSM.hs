module Shared.Api.Resource.Dev.DevSectionSM where

import Data.Swagger

import Shared.Api.Resource.Dev.DevOperationSM ()
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Api.Resource.Dev.DevSectionJM ()
import Shared.Database.Migration.Development.Dev.Data.Devs
import Shared.Util.Swagger

instance ToSchema DevSectionDTO where
  declareNamedSchema = toSwagger sectionDto
