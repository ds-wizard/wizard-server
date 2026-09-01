module Specs.Api.Handler.Role.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Role.Detail_DELETE
import Specs.Api.Handler.Role.Detail_GET
import Specs.Api.Handler.Role.Detail_PUT
import Specs.Api.Handler.Role.List_GET
import Specs.Api.Handler.Role.List_POST

roleAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER ROLE API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
