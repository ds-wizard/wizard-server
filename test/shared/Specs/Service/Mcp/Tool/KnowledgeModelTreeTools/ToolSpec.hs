module Specs.Service.Mcp.Tool.KnowledgeModelTreeTools.ToolSpec (
  knowledgeModelTreeToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelTreeTools.GetKnowledgeModelTree

knowledgeModelTreeToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelTreeToolsSpec requestContext = do
  get_knowledge_model_tree requestContext
