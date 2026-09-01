module Specs.Api.Handler.User.News.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.User.News.Detail_PUT

userNewsAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER NEWS API Spec" $
      detail_PUT requestContext
