module Specs.Api.Handler.ProjectFile.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.ProjectFile.List_GET

projectFileAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "PROJECT FILE API Spec" $ do
      list_GET requestContext
