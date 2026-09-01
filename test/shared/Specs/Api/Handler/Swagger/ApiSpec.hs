module Specs.Api.Handler.Swagger.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Swagger.List_GET

swaggerAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "SWAGGER API Spec" $ list_GET requestContext
