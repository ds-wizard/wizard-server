module Shared.Api.Resource.TypeHint.TypeHintISM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.TypeHint.TypeHintIJM ()
import Shared.Database.Migration.Development.TypeHint.Data.TypeHints
import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Util.Swagger

instance ToSchema TypeHintIDTO where
  declareNamedSchema = toSwagger genomicDatasetTypeHint
