module Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateAssetDTO where
  jsonSchema = genericJsonSchema jsonOptions
