module Shared.Api.Resource.Mcp.Guide.McpGuideTopicArgumentJM where

import Data.Aeson
import Data.List (find, intercalate)

import Shared.Api.Resource.Mcp.Guide.McpGuideTopicArgumentDTO
import Shared.Constant.Mcp.AuthoringGuide
import Shared.Model.Mcp.McpGuideTopic
import Shared.Util.JsonSchema

instance FromJSON McpGuideTopicArgumentDTO where
  parseJSON value = do
    name <- parseJSON value
    maybe (fail ("unknown topic, use one of " ++ intercalate ", " guideTopicNames)) (return . McpGuideTopicArgumentDTO) (find ((== name) . (.name)) authoringGuide)

instance JsonSchema McpGuideTopicArgumentDTO where
  jsonSchema _ = enumSchema guideTopicNames
