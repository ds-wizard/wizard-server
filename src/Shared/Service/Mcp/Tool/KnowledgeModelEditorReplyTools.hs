module Shared.Service.Mcp.Tool.KnowledgeModelEditorReplyTools where

import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO
import Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Editor.Reply.EditorReplyService
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools (editorUuidArgument)
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.ProjectTools (repliesChangeArguments)
import Shared.Util.JsonSchema

knowledgeModelEditorReplyTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelEditorReplyTools = [setKnowledgeModelEditorReplies]

setKnowledgeModelEditorReplies :: WizardRequestContextC s m => McpTool m
setKnowledgeModelEditorReplies =
  McpTool
    { name = "set_knowledge_model_editor_replies"
    , title = "Set knowledge model editor replies"
    , description =
        "Set the preview replies of a knowledge model editor, which document template previews of the editor use, by path "
          ++ "(get_authoring_guide topic document-context) and clear the replies of the paths in clear; replies at other paths stay as they are. "
          ++ "Every reply must fit the current knowledge model of the editor and the validations of its question; otherwise nothing is stored"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpRepliesChangeResultDTO)
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION]
    , toolset = Just DtMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ fmap toJSON . uncurry setEditorReplies
    }
  where
    arguments = (,) <$> editorUuidArgument <*> repliesChangeArguments
