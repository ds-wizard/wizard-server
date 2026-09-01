module Specs.Api.Handler.ExternalLink.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.ExternalLink.List_GET

externalLinkAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "EXTERNAL LINK API Spec" $ do
      list_GET requestContext
