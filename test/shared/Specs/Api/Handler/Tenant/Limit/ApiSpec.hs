module Specs.Api.Handler.Tenant.Limit.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Tenant.Limit.List_PUT

tenantLimitAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TENANT Limit API Spec" $
      list_PUT requestContext
