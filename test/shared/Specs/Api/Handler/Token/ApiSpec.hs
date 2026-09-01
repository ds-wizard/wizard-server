module Specs.Api.Handler.Token.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Token.Detail_DELETE
import Specs.Api.Handler.Token.List_Current_DELETE
import Specs.Api.Handler.Token.List_DELETE
import Specs.Api.Handler.Token.List_GET
import Specs.Api.Handler.Token.List_POST

tokenAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "TOKEN API Spec" $ do
    list_GET requestContext
    list_POST requestContext
    list_DELETE requestContext
    list_current_DELETE requestContext
    detail_DELETE requestContext
