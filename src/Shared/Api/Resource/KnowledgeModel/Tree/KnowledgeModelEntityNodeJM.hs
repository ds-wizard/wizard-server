module Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeJM where

import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntityJM
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelEntityNodeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelEntityNodeDTO where
  parseJSON = withObject "KnowledgeModelEntityNodeDTO" $ \o -> do
    entityType <- o .: "entityType"
    KnowledgeModelEntityNodeDTO
      <$> o .: "uuid"
      <*> o .:? "parentUuid"
      <*> pure entityType
      <*> o .: "title"
      <*> o .: "depth"
      <*> (traverse (parseEntity entityType) =<< o .:? "content")

instance JsonSchema KnowledgeModelEntityNodeDTO where
  jsonSchema _ = definitionSchema (fst knowledgeModelEntityNodeDefinition)

knowledgeModelEntityNodeDefinition :: (String, Value)
knowledgeModelEntityNodeDefinition = ("knowledgeModelEntityNode", genericJsonSchema jsonOptions (Proxy :: Proxy KnowledgeModelEntityNodeDTO))
