module Shared.Api.Resource.Mcp.KnowledgeModel.McpEventTypeArgumentJM where

import Data.Aeson
import Data.List (intercalate)

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM (knowledgeModelEventTypes)
import Shared.Api.Resource.Mcp.KnowledgeModel.McpEventTypeArgumentDTO
import Shared.Util.JsonSchema

instance FromJSON McpEventTypeArgumentDTO where
  parseJSON value = do
    eventType <- parseJSON value
    if eventType `elem` knowledgeModelEventTypes
      then return (McpEventTypeArgumentDTO eventType)
      else fail ("unknown event type " ++ show eventType ++ ", use one of " ++ intercalate ", " knowledgeModelEventTypes)

instance JsonSchema McpEventTypeArgumentDTO where
  jsonSchema _ = enumSchema knowledgeModelEventTypes
