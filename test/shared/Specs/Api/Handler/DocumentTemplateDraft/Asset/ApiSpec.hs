module Specs.Api.Handler.DocumentTemplateDraft.Asset.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_DELETE
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_GET
import Specs.Api.Handler.DocumentTemplateDraft.Asset.Detail_PUT
import Specs.Api.Handler.DocumentTemplateDraft.Asset.List_GET

documentTemplateDraftAssetAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT TEMPLATE DRAFT ASSET API Spec" $ do
      list_GET requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
