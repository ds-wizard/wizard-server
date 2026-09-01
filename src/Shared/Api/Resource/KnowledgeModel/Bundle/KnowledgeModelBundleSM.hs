module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Util.Swagger

instance ToSchema KnowledgeModelBundle where
  declareNamedSchema = toSwagger netherlandsV2KmBundle
