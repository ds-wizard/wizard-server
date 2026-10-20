module Shared.Service.Mcp.Tool.AuthoringGuideTools where

import Data.Aeson

import Shared.Api.Resource.Mcp.Guide.McpGuideTopicArgumentDTO
import Shared.Api.Resource.Mcp.Guide.McpGuideTopicArgumentJM ()
import Shared.Constant.Mcp.AuthoringGuide
import Shared.Model.Context.TransactionState
import Shared.Model.Mcp.McpGuideTopic
import Shared.Model.Mcp.McpTool
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema

authoringGuideTools :: Monad m => [McpTool m]
authoringGuideTools = [getAuthoringGuide]

getAuthoringGuide :: Monad m => McpTool m
getAuthoringGuide =
  McpTool
    { name = "get_authoring_guide"
    , title = "Get authoring guide"
    , description =
        "Get one topic of the authoring guide: workflow (the recommended order of work, scope and confirmations), knowledge-model-metamodel, knowledge-model-events, "
          ++ "document-template-structure (formats and their steps with options), document-context (what a template receives) or template-filters. "
          ++ "Read the topics you need before changing knowledge models or document templates"
    , inputSchema = argumentsSchema topicArgument
    , outputSchema = Just $ objectSchema ["topic", "title", "content"] [("topic", enumSchema guideTopicNames), ("title", stringSchema), ("content", describedSchema "Markdown" stringSchema)]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Nothing
    , transactionState = NoTransaction
    , run = withArguments topicArgument (return . toGuideJSON)
    }

topicArgument :: Arguments McpGuideTopicArgumentDTO
topicArgument = requiredArgument "topic" "The topic of the guide"

toGuideJSON :: McpGuideTopicArgumentDTO -> Value
toGuideJSON (McpGuideTopicArgumentDTO topic) = object ["topic" .= topic.name, "title" .= topic.title, "content" .= topic.content]
