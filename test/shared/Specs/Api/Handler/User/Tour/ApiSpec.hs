module Specs.Api.Handler.User.Tour.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Tour.Detail_PUT
import Specs.Api.Handler.User.Tour.List_DELETE

userTourAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER TOUR API Spec" $ do
      list_DELETE requestContext
      detail_PUT requestContext
