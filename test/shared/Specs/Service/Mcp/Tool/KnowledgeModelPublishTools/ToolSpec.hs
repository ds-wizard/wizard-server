module Specs.Service.Mcp.Tool.KnowledgeModelPublishTools.ToolSpec (
  knowledgeModelPublishToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelPublishTools.PublishKnowledgeModelEditor

knowledgeModelPublishToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelPublishToolsSpec requestContext = do
  publish_knowledge_model_editor requestContext
