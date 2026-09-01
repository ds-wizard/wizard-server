module Specs.Api.Handler.Domain.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Domain.Detail_GET

domainAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "DOMAIN API Spec" $ detail_GET requestContext
