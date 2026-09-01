module Specs.Api.Handler.KnowledgeModelPackage.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelPackage.Dependent.ApiSpec
import Specs.Api.Handler.KnowledgeModelPackage.Detail_DELETE
import Specs.Api.Handler.KnowledgeModelPackage.Detail_GET
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_Content_GET
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_DELETE
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_GET
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_POST
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Locales_Template_GET
import Specs.Api.Handler.KnowledgeModelPackage.Detail_PUT
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Pull_POST
import Specs.Api.Handler.KnowledgeModelPackage.List_From_Editor_POST
import Specs.Api.Handler.KnowledgeModelPackage.List_GET
import Specs.Api.Handler.KnowledgeModelPackage.List_POST
import Specs.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET

knowledgeModelPackageAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "KNOWLEDGE MODEL PACKAGE API Spec" $ do
      list_GET requestContext
      list_suggestions_GET requestContext
      list_POST requestContext
      list_from_editor_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_pull_POST requestContext
      detail_locales_GET requestContext
      detail_locales_POST requestContext
      detail_locales_template_GET requestContext
      detail_locales_content_GET requestContext
      detail_locales_DELETE requestContext
      dependentAPI requestContext
