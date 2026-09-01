module Specs.Api.Handler.Submission.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Submission.List_GET
import Specs.Api.Handler.Submission.List_POST

submissionAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "SUBMISSION API Spec" $ do
      list_GET requestContext
      list_POST requestContext
