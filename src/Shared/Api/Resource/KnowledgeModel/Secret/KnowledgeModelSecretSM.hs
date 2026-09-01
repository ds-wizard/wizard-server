module Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Util.Swagger

instance ToSchema KnowledgeModelSecret where
  declareNamedSchema = toSwagger kmSecret1
