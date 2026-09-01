module Specs.Api.Handler.UserGroup.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Detail_GET
import Specs.Api.Handler.UserGroup.List_Suggestions_GET

userGroupAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER GROUP API Spec" $ do
      list_suggestions_GET requestContext
      detail_GET requestContext
