module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM where

import Data.Aeson (toJSON)
import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

instance ToSchema KnowledgeModelBundlePackage where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "KnowledgeModelBundlePackage") (mempty {_schemaExample = Just . toJSON $ globalKmBundlePackage})
