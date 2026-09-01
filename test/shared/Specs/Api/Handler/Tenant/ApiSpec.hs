module Specs.Api.Handler.Tenant.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Tenant.Detail_GET
import Specs.Api.Handler.Tenant.Detail_PUT
import Specs.Api.Handler.Tenant.List_GET
import Specs.Api.Handler.Tenant.List_POST
import Specs.Api.Handler.Tenant.PluginSettings.ApiSpec

tenantAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TENANT API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      tenantPluginSettingsAPI requestContext
