module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.ToolSpec (
  knowledgeModelMigrationToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.CancelKnowledgeModelMigration
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.GetKnowledgeModelMigration
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.MigrateProject
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.ResolveKnowledgeModelMigration
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.StartKnowledgeModelMigration

knowledgeModelMigrationToolsSpec :: RequestContext -> SpecWith ((), Application)
knowledgeModelMigrationToolsSpec requestContext = do
  start_knowledge_model_migration requestContext
  get_knowledge_model_migration requestContext
  resolve_knowledge_model_migration requestContext
  cancel_knowledge_model_migration requestContext
  migrate_project requestContext
