module Shared.Api.Resource.Dev.DevSM where

import Data.Swagger

import Shared.Api.Resource.Dev.DevJM ()
import Shared.Database.Migration.Development.Dev.Data.Devs
import Shared.Model.Dev.Dev
import Shared.Util.Swagger

instance ToSchema DevOperationParameter where
  declareNamedSchema = toSwagger operationParam1

instance ToSchema DevOperationParameterType
