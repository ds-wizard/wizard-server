module Shared.Api.Resource.TemporaryFile.TemporaryFileSM where

import Data.Swagger

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Util.Swagger

instance ToSchema TemporaryFileDTO where
  declareNamedSchema = toSwagger $ TemporaryFileDTO {url = "http://example.com/temporary-file-1", contentType = "text/plain"}
