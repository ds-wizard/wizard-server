module Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateFileList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentTemplateFileList where
  jsonSchema = genericJsonSchema jsonOptions
