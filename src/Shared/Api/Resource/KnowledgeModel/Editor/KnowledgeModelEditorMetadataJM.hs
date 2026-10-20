module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorMetadataJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorMetadataDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelEditorMetadataDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorMetadataDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelEditorMetadataDTO where
  jsonSchema = genericJsonSchema jsonOptions
