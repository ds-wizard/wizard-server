module Specs.Api.Handler.Locale.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.Locale.Detail_DELETE
import Specs.Api.Handler.Locale.Detail_GET
import Specs.Api.Handler.Locale.Detail_PUT
import Specs.Api.Handler.Locale.List_Current_Content_GET
import Specs.Api.Handler.Locale.List_DELETE
import Specs.Api.Handler.Locale.List_GET
import Specs.Api.Handler.Locale.List_Suggestions_GET

localeAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "LOCALE API Spec" $ do
      list_GET requestContext
      list_suggestions_GET requestContext
      list_current_content_GET requestContext
      list_DELETE requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
