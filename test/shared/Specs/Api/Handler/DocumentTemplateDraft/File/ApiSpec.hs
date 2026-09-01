module Specs.Api.Handler.DocumentTemplateDraft.File.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.File.Detail_DELETE
import Specs.Api.Handler.DocumentTemplateDraft.File.Detail_GET
import Specs.Api.Handler.DocumentTemplateDraft.File.Detail_PUT
import Specs.Api.Handler.DocumentTemplateDraft.File.List_GET
import Specs.Api.Handler.DocumentTemplateDraft.File.List_POST

documentTemplateDraftFileAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT TEMPLATE DRAFT FILE API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
