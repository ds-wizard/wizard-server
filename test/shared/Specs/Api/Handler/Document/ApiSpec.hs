module Specs.Api.Handler.Document.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Document.Detail_Available_Submission_Services_GET
import Specs.Api.Handler.Document.Detail_DELETE
import Specs.Api.Handler.Document.List_GET
import Specs.Api.Handler.Document.List_POST

documentAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "DOCUMENT API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_DELETE requestContext
      detail_available_submission_Services_GET requestContext
