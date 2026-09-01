module Specs.Api.Handler.User.PluginSettings.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.User.PluginSettings.Detail_GET
import Specs.Api.Handler.User.PluginSettings.Detail_PUT

userPluginSettingsAPI requestContext =
  describe "USER PLUGIN SETTINGS API Spec" $ do
    detail_GET requestContext
    detail_PUT requestContext
