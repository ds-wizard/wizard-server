module Specs.Api.Handler.TypeHint.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.TypeHint.List_POST
import Specs.Api.Handler.TypeHint.Test_POST

typeHintAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TYPEHINT API Spec" $ do
      list_POST requestContext
      test_POST requestContext
