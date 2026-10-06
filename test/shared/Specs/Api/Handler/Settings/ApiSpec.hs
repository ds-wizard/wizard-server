module Specs.Api.Handler.Settings.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Settings.List_DELETE
import Specs.Api.Handler.Settings.List_GET
import Specs.Api.Handler.Settings.List_PUT

settingsAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "SETTINGS API Spec" $ do
      list_GET requestContext
      list_PUT requestContext
      list_DELETE requestContext
