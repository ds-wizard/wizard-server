module Specs.Api.Handler.Prefab.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Prefab.List_GET

prefabAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "PREFAB API Spec" $ list_GET requestContext
