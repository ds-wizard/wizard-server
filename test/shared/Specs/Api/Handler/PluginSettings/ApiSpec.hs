module Specs.Api.Handler.PluginSettings.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.PluginSettings.Detail_DELETE
import Specs.Api.Handler.PluginSettings.Detail_GET
import Specs.Api.Handler.PluginSettings.Detail_PUT
import Specs.Api.Handler.PluginSettings.List_GET
import Specs.Api.Handler.PluginSettings.List_PUT

pluginSettingsAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "PLUGIN SETTINGS API Spec" $ do
      list_GET requestContext
      list_PUT requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
