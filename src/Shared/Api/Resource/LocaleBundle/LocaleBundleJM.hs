module Shared.Api.Resource.LocaleBundle.LocaleBundleJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Coordinate.CoordinateJM
import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Util.Aeson

instance FromJSON LocaleBundleDTO where
  parseJSON (Object o) = do
    id <- parseLegacyId o "localeId"
    name <- o .: "name"
    description <- o .: "description"
    code <- o .: "code"
    version <- o .: "version"
    license <- o .: "license"
    readme <- o .: "readme"
    recommendedAppVersion <- o .: "recommendedAppVersion"
    createdAt <- o .: "createdAt"
    return LocaleBundleDTO {..}
  parseJSON _ = mzero

instance ToJSON LocaleBundleDTO where
  toJSON = genericToJSON jsonOptions
