module Specs.Api.Handler.KnowledgeModelSecret.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelSecret.Detail_DELETE
import Specs.Api.Handler.KnowledgeModelSecret.Detail_PUT
import Specs.Api.Handler.KnowledgeModelSecret.List_GET
import Specs.Api.Handler.KnowledgeModelSecret.List_POST

knowledgeModelSecretAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "KNOWLEDGE MODEL SECRET API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
