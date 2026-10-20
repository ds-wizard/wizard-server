module Specs.Service.Mcp.Tool.KnowledgeModelEventTools.ToolSpec (
  knowledgeModelEventToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelEventTools.DescribeKnowledgeModelEvents
import Specs.Service.Mcp.Tool.KnowledgeModelEventTools.EditKnowledgeModel

knowledgeModelEventToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelEventToolsSpec requestContext = do
  describe_knowledge_model_events requestContext
  edit_knowledge_model requestContext
