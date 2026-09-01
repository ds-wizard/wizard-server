module Shared.Api.Resource.Dev.DevExecutionSM where

import Data.Swagger

import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Api.Resource.Dev.DevExecutionJM ()
import Shared.Database.Migration.Development.Dev.Data.Devs
import Shared.Util.Swagger

instance ToSchema DevExecutionDTO where
  declareNamedSchema = toSwagger execution1
