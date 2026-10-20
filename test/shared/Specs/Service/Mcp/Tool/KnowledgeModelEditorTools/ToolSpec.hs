module Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.ToolSpec (
  knowledgeModelEditorToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.DeleteKnowledgeModelEditor
import Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.ListKnowledgeModelEditors
import Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.SaveKnowledgeModelEditor

knowledgeModelEditorToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelEditorToolsSpec requestContext = do
  list_knowledge_model_editors requestContext
  save_knowledge_model_editor requestContext
  delete_knowledge_model_editor requestContext
