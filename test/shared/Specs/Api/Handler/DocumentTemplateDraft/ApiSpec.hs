module Specs.Api.Handler.DocumentTemplateDraft.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Detail_DELETE
import Specs.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT
import Specs.Api.Handler.DocumentTemplateDraft.Detail_GET
import Specs.Api.Handler.DocumentTemplateDraft.Detail_PUT
import Specs.Api.Handler.DocumentTemplateDraft.List_GET
import Specs.Api.Handler.DocumentTemplateDraft.List_POST

documentTemplateDraftAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT TEMPLATE DRAFT API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_documents_preview_settings_PUT requestContext
