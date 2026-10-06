module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Coordinate.CoordinateJM
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Util.Aeson

instance FromJSON KnowledgeModelBundle where
  parseJSON (Object o) = do
    id <- parseLegacyId o "kmId"
    name <- o .: "name"
    version <- o .: "version"
    metamodelVersion <- o .: "metamodelVersion"
    packages <- o .: "packages"
    return KnowledgeModelBundle {..}
  parseJSON _ = mzero

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
