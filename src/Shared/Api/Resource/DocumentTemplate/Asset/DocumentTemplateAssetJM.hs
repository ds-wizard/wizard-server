module Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
