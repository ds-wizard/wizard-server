module Shared.Api.Resource.Config.SimpleFeatureSM where

import Data.Swagger

import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Model.Config.SimpleFeature
import Shared.Util.Swagger

instance ToSchema SimpleFeature where
  declareNamedSchema = toSwagger (SimpleFeature True)
