module Specs.Api.Handler.KnowledgeModelEditor.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Detail_DELETE
import Specs.Api.Handler.KnowledgeModelEditor.Detail_GET
import Specs.Api.Handler.KnowledgeModelEditor.Detail_Locales_GET
import Specs.Api.Handler.KnowledgeModelEditor.Detail_PUT
import Specs.Api.Handler.KnowledgeModelEditor.List_GET
import Specs.Api.Handler.KnowledgeModelEditor.List_POST
import Specs.Api.Handler.KnowledgeModelEditor.Migration.ApiSpec

knowledgeModelEditorAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "KNOWLEDGE MODEL EDITOR API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_locales_GET requestContext
      knowledgeModelEditorMigrationAPI requestContext
