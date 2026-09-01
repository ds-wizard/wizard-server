module Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateAssetChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetChangeDTO where
  toJSON = genericToJSON jsonOptions
