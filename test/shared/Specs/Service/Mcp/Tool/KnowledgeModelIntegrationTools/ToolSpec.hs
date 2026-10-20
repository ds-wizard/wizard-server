module Specs.Service.Mcp.Tool.KnowledgeModelIntegrationTools.ToolSpec (
  knowledgeModelIntegrationToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelIntegrationTools.TestKnowledgeModelIntegration

knowledgeModelIntegrationToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelIntegrationToolsSpec requestContext = do
  test_knowledge_model_integration requestContext
