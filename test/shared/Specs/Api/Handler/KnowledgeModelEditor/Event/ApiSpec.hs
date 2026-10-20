module Specs.Api.Handler.KnowledgeModelEditor.Event.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.KnowledgeModelEditor.Event.List_POST

knowledgeModelEditorEventAPI requestContext =
  describe "KNOWLEDGE MODEL EDITOR EVENT API Spec" $
    list_POST requestContext
