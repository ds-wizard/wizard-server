module Shared.Api.Resource.Tenant.Usage.UsageEntrySM where

import Data.Swagger

import Shared.Api.Resource.Tenant.Usage.UsageEntryDTO
import Shared.Api.Resource.Tenant.Usage.UsageEntryJM ()
import Shared.Util.Swagger

instance ToSchema UsageEntryDTO where
  declareNamedSchema = toSwagger (UsageEntryDTO {current = 1, max = 10})
