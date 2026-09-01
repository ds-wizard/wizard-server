module Specs.Api.Handler.Config.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Config.List_Bootstrap_GET

configAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "CONFIG API Spec" $
      list_bootstrap_GET requestContext
