module Shared.Api.Resource.Dev.DevOperationSM where

import Data.Swagger

import Shared.Api.Resource.Dev.DevOperationDTO
import Shared.Api.Resource.Dev.DevOperationJM ()
import Shared.Api.Resource.Dev.DevSM ()
import Shared.Database.Migration.Development.Dev.Data.Devs
import Shared.Util.Swagger

instance ToSchema DevOperationDTO where
  declareNamedSchema = toSwagger operationDto
