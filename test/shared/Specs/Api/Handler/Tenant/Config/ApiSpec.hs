module Specs.Api.Handler.Tenant.Config.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Tenant.Config.List_GET
import Specs.Api.Handler.Tenant.Config.List_PUT

tenantConfigAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TENANT CONFIG API Spec" $ do
      list_GET requestContext
      list_PUT requestContext
