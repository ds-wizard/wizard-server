module Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeJM
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelTreeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelTreeDTO where
  jsonSchema = withDefinitions [knowledgeModelEntityNodeDefinition] . genericJsonSchema jsonOptions
