module Specs.Api.Handler.Tenant.PluginSettings.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Tenant.PluginSettings.Detail_GET
import Specs.Api.Handler.Tenant.PluginSettings.Detail_PUT
import Specs.Api.Handler.Tenant.PluginSettings.List_PUT

tenantPluginSettingsAPI requestContext =
  describe "TENANT PLUGIN SETTINGS API Spec" $ do
    list_PUT requestContext
    detail_GET requestContext
    detail_PUT requestContext
