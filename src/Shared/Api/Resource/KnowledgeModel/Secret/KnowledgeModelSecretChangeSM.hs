module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Util.Swagger

instance ToSchema KnowledgeModelSecretChangeDTO where
  declareNamedSchema = toSwagger kmSecret1ChangeDTO
