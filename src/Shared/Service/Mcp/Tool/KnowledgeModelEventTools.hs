module Shared.Service.Mcp.Tool.KnowledgeModelEventTools where

import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM
import Shared.Api.Resource.Mcp.KnowledgeModel.McpEventTypeArgumentDTO
import Shared.Api.Resource.Mcp.KnowledgeModel.McpEventTypeArgumentJM ()
import Shared.Constant.Mcp.KnowledgeModelEventExamples
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventService
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools (editorUuidArgument)
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema

knowledgeModelEventTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelEventTools = [editKnowledgeModel]

describeKnowledgeModelEvents :: Monad m => McpTool m
describeKnowledgeModelEvents =
  McpTool
    { name = "describe_knowledge_model_events"
    , title = "Describe knowledge model events"
    , description =
        "Get the exact JSON schema of the content of the given knowledge model event types and an example event of each, "
          ++ "in the shape edit_knowledge_model takes. Ask only for the types you are about to send"
    , inputSchema = argumentsSchema typesArgument
    , outputSchema = Just $ objectSchema ["events"] [("events", arraySchema eventDescriptionSchema)]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Nothing
    , transactionState = NoTransaction
    , run = withArguments typesArgument (\eventTypes -> return $ object ["events" .= fmap describeEventType eventTypes])
    }

typesArgument :: Arguments [McpEventTypeArgumentDTO]
typesArgument = requiredArgument "types" "The event types to describe, for example AddQuestionEvent"

eventDescriptionSchema :: Value
eventDescriptionSchema =
  objectSchema
    ["eventType", "schema", "example"]
    [ ("eventType", enumSchema knowledgeModelEventTypes)
    , ("schema", describedSchema "The JSON schema of the content of the event" (object []))
    , ("example", jsonSchema (Proxy :: Proxy KnowledgeModelEditorEventChangeDTO))
    ]

describeEventType :: McpEventTypeArgumentDTO -> Value
describeEventType (McpEventTypeArgumentDTO eventType) =
  object
    [ "eventType" .= eventType
    , "schema" .= fromMaybe Null (lookup eventType knowledgeModelEventSchemas)
    , "example" .= fmap toEventChangeDTO (find ((== eventType) . eventTypeName . (.content)) knowledgeModelEventExamples)
    ]

editKnowledgeModel :: forall s m. WizardRequestContextC s m => McpTool m
editKnowledgeModel =
  markThirdPartyFields [["outline"]] $
    McpTool
      { name = "edit_knowledge_model"
      , title = "Edit knowledge model"
      , description =
          "Change the knowledge model of an editor with a batch of events (get_authoring_guide topic knowledge-model-events; "
            ++ "describe_knowledge_model_events for the exact fields of an event type). "
            ++ "Name new entities $name and use the names in later events of the batch; the result maps every name to its UUID. "
            ++ "The batch is validated against the knowledge model compiled up to every event and stored completely or not at all; "
            ++ "every error comes back at once with the event index and the field. Validate with dry_run first. "
            ++ "Give base_revision, the revision of the previous result, so that the batch is refused when someone changed the editor since; "
            ++ "repeating an already stored batch with the same base_revision succeeds without storing it twice"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelEditorEventsDTO)
      , annotations = writeAnnotations {destructiveHint = True}
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(editorUuid, reqDto) -> toJSON <$> createEditorEvents editorUuid reqDto
      }
  where
    arguments = (,) <$> editorUuidArgument <*> eventsArguments :: Arguments (U.UUID, KnowledgeModelEditorEventsChangeDTO)

eventsArguments :: Arguments KnowledgeModelEditorEventsChangeDTO
eventsArguments =
  KnowledgeModelEditorEventsChangeDTO
    <$> requiredArgument "events" "The events, applied in order"
    <*> optionalArgument "base_revision" "The revision your batch builds on, from the previous result"
    <*> optionalArgument "dry_run" "Validate the batch and return the result without storing anything"
