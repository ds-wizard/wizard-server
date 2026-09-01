module Shared.Api.Resource.Dev.DevExecutionResultSM where

import Data.Swagger

import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Api.Resource.Dev.DevExecutionResultJM ()
import Shared.Database.Migration.Development.Dev.Data.Devs
import Shared.Util.Swagger

instance ToSchema AdminExecutionResultDTO where
  declareNamedSchema = toSwagger execution1Result
