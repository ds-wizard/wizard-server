module Shared.Api.Resource.Common.PageMetadataJM where

import Data.Aeson

import Shared.Model.Common.PageMetadata
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON PageMetadata where
  toJSON = genericToJSON jsonOptions

instance JsonSchema PageMetadata where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON PageMetadata where
  parseJSON = genericParseJSON jsonOptions
