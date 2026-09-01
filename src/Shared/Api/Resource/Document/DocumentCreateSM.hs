module Shared.Api.Resource.Document.DocumentCreateSM where

import Data.Swagger

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentCreateJM ()
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Util.Swagger

instance ToSchema DocumentCreateDTO where
  declareNamedSchema = toSwagger doc1Create
