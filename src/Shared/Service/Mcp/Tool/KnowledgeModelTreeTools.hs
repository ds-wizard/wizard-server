module Shared.Service.Mcp.Tool.KnowledgeModelTreeTools where

import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeJM ()
import Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentDTO
import Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentJM ()
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelTreeQuery
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeService
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema

knowledgeModelTreeTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelTreeTools = [getKnowledgeModelTree]

getKnowledgeModelTree :: forall s m. WizardRequestContextC s m => McpTool m
getKnowledgeModelTree =
  markThirdPartyFields [["entities"]] $
    McpTool
      { name = "get_knowledge_model_tree"
      , title = "Get knowledge model tree"
      , description =
          "Get the current knowledge model of an editor (with its unpublished changes) or of a package as a flat list of entities in tree order, "
            ++ "each with its parent and depth: the whole model or the subtree of one entity, optionally only some levels deep and only with questions of some tags. "
            ++ "Without content it is an outline (type and title); with content every entity comes as in the knowledge model JSON"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelTreeDTO)
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just KmMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ \(getTree, query) -> toJSON <$> getTree query
      }
  where
    arguments = treeArguments :: Arguments (KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO, KnowledgeModelTreeQuery)

treeArguments :: WizardRequestContextC s m => Arguments (KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO, KnowledgeModelTreeQuery)
treeArguments = (,) <$> treeSourceArguments <*> treeQueryArguments

treeSourceArguments :: WizardRequestContextC s m => Arguments (KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO)
treeSourceArguments =
  checkArguments treeSource $
    (,)
      <$> optionalArgument "knowledge_model_editor_uuid" "The editor whose knowledge model to read, with its unpublished changes"
      <*> optionalArgument "knowledge_model_package_uuid" "The package whose knowledge model to read"

treeSource :: WizardRequestContextC s m => (Maybe U.UUID, Maybe U.UUID) -> Either LocaleRecord (KnowledgeModelTreeQuery -> m KnowledgeModelTreeDTO)
treeSource (Just editorUuid, Nothing) = Right (getEditorKnowledgeModelTree editorUuid)
treeSource (Nothing, Just pkgUuid) = Right (getPackageKnowledgeModelTree pkgUuid)
treeSource _ = Left $ _ERROR_VALIDATION__MCP_ONE_OF_ARGUMENTS "knowledge_model_editor_uuid" "knowledge_model_package_uuid"

treeQueryArguments :: Arguments KnowledgeModelTreeQuery
treeQueryArguments =
  KnowledgeModelTreeQuery
    <$> optionalArgument "entity_uuid" "The entity (a chapter, a question or any other) whose subtree to return; the whole knowledge model by default"
    <*> (fmap (\(McpNonNegativeArgumentDTO depth) -> depth) <$> optionalArgument "depth" "How many levels below the entity to include; all by default")
    <*> (fromMaybe [] <$> optionalArgument "tag_uuids" "Keep only the questions with at least one of these tags")
    <*> (fromMaybe False <$> optionalArgument "content" "Return the full content of every entity, not only its type and title")
