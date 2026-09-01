module Specs.Api.Handler.ProjectCommentThread.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.ProjectCommentThread.List_GET

projectCommentThreadAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "PROJECT COMMENT THREAD API Spec" $ do
      list_GET requestContext
