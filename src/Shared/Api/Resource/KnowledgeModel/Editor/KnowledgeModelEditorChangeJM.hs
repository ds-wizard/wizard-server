module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelEditorChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelEditorChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
