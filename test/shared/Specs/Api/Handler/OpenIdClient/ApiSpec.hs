module Specs.Api.Handler.OpenIdClient.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.OpenIdClient.Detail_DELETE
import Specs.Api.Handler.OpenIdClient.Detail_GET
import Specs.Api.Handler.OpenIdClient.Detail_PUT
import Specs.Api.Handler.OpenIdClient.List_GET
import Specs.Api.Handler.OpenIdClient.List_POST

openIdClientAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "OPEN ID CLIENT API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
