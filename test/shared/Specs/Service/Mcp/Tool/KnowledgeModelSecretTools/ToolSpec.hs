module Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.ToolSpec (
  knowledgeModelSecretToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.DeleteKnowledgeModelSecret
import Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.ListKnowledgeModelSecrets
import Specs.Service.Mcp.Tool.KnowledgeModelSecretTools.SaveKnowledgeModelSecret

knowledgeModelSecretToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelSecretToolsSpec requestContext = do
  list_knowledge_model_secrets requestContext
  save_knowledge_model_secret requestContext
  delete_knowledge_model_secret requestContext
