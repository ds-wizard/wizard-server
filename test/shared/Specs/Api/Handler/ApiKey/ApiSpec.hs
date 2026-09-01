module Specs.Api.Handler.ApiKey.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.ApiKey.Detail_DELETE
import Specs.Api.Handler.ApiKey.List_GET
import Specs.Api.Handler.ApiKey.List_POST
import Specs.Api.Handler.Common

apiKeyAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "API KEY API Spec" $ do
    list_GET requestContext
    list_POST requestContext
    detail_DELETE requestContext
