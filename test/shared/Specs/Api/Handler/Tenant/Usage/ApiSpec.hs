module Specs.Api.Handler.Tenant.Usage.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Tenant.Usage.Current_Wizard_GET
import Specs.Api.Handler.Tenant.Usage.Detail_Wizard_GET

usageAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USAGE API Spec" $ do
      current_wizard_GET requestContext
      detail_wizard_GET requestContext
