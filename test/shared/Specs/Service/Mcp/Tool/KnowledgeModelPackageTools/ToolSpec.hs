module Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.ToolSpec (
  knowledgeModelPackageToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.DeleteKnowledgeModelPackage
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.ExportKnowledgeModelPackage
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.GetKnowledgeModelPackage
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.ImportKnowledgeModelPackage
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.ListKnowledgeModelPackages
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.PullKnowledgeModelPackage
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.SearchRegistryPackages
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.UpdateKnowledgeModelPackage

knowledgeModelPackageToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelPackageToolsSpec requestContext = do
  list_knowledge_model_packages requestContext
  get_knowledge_model_package requestContext
  search_registry_packages requestContext
  pull_knowledge_model_package requestContext
  update_knowledge_model_package requestContext
  delete_knowledge_model_package requestContext
  export_knowledge_model_package requestContext
  import_knowledge_model_package requestContext
