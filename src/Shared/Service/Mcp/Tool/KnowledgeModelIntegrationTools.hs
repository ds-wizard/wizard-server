module Shared.Service.Mcp.Tool.KnowledgeModelIntegrationTools where

import Data.Aeson (toJSON)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools (editorUuidArgument)
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon (writeAnnotations)
import Shared.Service.TypeHint.TypeHintService
import Shared.Util.JsonSchema

knowledgeModelIntegrationTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelIntegrationTools = [testKnowledgeModelIntegration]

testKnowledgeModelIntegration :: forall s m. WizardRequestContextC s m => McpTool m
testKnowledgeModelIntegration =
  markThirdPartyFields [["response"]] $
    McpTool
      { name = "test_knowledge_model_integration"
      , title = "Test knowledge model integration"
      , description =
          "Call an API integration of a knowledge model editor once and return the request it sent (with secrets masked) and the response it got, "
            ++ "to tune the request and the response templates before you store them. pending_events, in the shape edit_knowledge_model takes, "
            ++ "are applied to the editor in memory first and never stored, and integration_uuid may be a $name one of them creates. "
            ++ "variables and q default to the test values of the integration. Secrets are filled in only into an integration as the editor stores it, "
            ++ "which needs mcp.sensitive, and are masked in the response; an integration created or changed by pending_events is called without secrets"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy TypeHintExchange)
      , annotations = writeAnnotations {openWorldHint = True}
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments $ \(editorUuid, integrationUuid, mPendingEvents, mVariables, mQ) ->
          toJSON <$> testTypeHintsWithPendingEvents editorUuid (fromMaybe [] mPendingEvents) integrationUuid mVariables mQ
      }
  where
    arguments =
      (,,,,)
        <$> editorUuidArgument
        <*> requiredArgument "integration_uuid" "The API integration to call: its UUID, or a $name created by pending_events"
        <*> optionalArgument "pending_events" "Events applied to the editor in memory before the call, as for edit_knowledge_model"
        <*> optionalArgument "variables" "The values of the variables of the integration"
        <*> optionalArgument "q" "The search text"
        :: Arguments (U.UUID, String, Maybe [KnowledgeModelEditorEventChangeDTO], Maybe (M.Map String String), Maybe String)
