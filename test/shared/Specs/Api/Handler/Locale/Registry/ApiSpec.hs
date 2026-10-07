module Specs.Api.Handler.Locale.Registry.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Integration.Http.Registry.Common

import Specs.Api.Handler.Locale.Registry.Detail_Pull_POST
import Specs.Api.Handler.Locale.Registry.List_GET

localeRegistryAPI serverContext requestContext = do
  fakeServerContext <- runIO $ withFakeRegistryServer serverContext
  with (startWebApp fakeServerContext requestContext) $
    describe "LOCALE REGISTRY API Spec" $ do
      list_GET requestContext
      detail_pull_POST requestContext
