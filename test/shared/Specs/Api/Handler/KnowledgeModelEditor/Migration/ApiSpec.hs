module Specs.Api.Handler.KnowledgeModelEditor.Migration.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_All_POST
import Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_POST
import Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_DELETE
import Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_GET
import Specs.Api.Handler.KnowledgeModelEditor.Migration.List_Current_POST

knowledgeModelEditorMigrationAPI requestContext =
  describe "KNOWLEDGE MODEL EDITOR MIGRATION API Spec" $ do
    list_current_GET requestContext
    list_current_POST requestContext
    list_current_DELETE requestContext
    list_current_conflict_POST requestContext
    list_Current_Conflict_All_POST requestContext
