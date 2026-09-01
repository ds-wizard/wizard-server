module Shared.Api.Resource.Version.VersionSM where

import Data.Swagger

import Shared.Api.Resource.Version.VersionDTO
import Shared.Api.Resource.Version.VersionJM ()
import Shared.Service.Version.VersionMapper
import Shared.Util.Swagger
import Shared.Util.Uuid

instance ToSchema VersionDTO where
  declareNamedSchema =
    toSwagger
      (toVersionDTO (u' "ac3a6934-2069-4792-943c-e1170edee8c2", "1.0.0"))
