module Specs.Api.Handler.Info.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Info.List_GET

infoAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "INFO API Spec" $ list_GET requestContext
