module Specs.Service.Mcp.Tool.KnowledgeModelEditorReplyTools.ToolSpec (
  knowledgeModelEditorReplyToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelEditorReplyTools.SetKnowledgeModelEditorReplies

knowledgeModelEditorReplyToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelEditorReplyToolsSpec requestContext = do
  set_knowledge_model_editor_replies requestContext
