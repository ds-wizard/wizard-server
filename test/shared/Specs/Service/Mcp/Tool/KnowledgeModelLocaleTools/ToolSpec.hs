module Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.ToolSpec (
  knowledgeModelLocaleToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.DeleteKnowledgeModelLocale
import Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.GetKnowledgeModelTranslation
import Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.SaveKnowledgeModelLocale

knowledgeModelLocaleToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelLocaleToolsSpec requestContext = do
  get_knowledge_model_translation requestContext
  save_knowledge_model_locale requestContext
  delete_knowledge_model_locale requestContext
