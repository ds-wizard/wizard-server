module Specs.Api.Handler.UserGroup.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Detail_DELETE
import Specs.Api.Handler.UserGroup.Detail_GET
import Specs.Api.Handler.UserGroup.Detail_PUT
import Specs.Api.Handler.UserGroup.List_GET
import Specs.Api.Handler.UserGroup.List_POST
import Specs.Api.Handler.UserGroup.List_Suggestions_GET
import Specs.Api.Handler.UserGroup.Membership.List_PUT

userGroupAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER GROUP API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      list_suggestions_GET requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      list_PUT requestContext
