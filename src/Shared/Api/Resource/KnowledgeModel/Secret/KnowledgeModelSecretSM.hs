module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper
import Shared.Util.Swagger

instance ToSchema KnowledgeModelSecretDTO where
  declareNamedSchema = toSwagger (toDTO kmSecret1)
