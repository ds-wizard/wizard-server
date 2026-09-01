module Specs.Api.Handler.Project.Comment.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.Comment.List_GET

projectCommentAPI requestContext =
  describe "PROJECT COMMENT API Spec" $
    list_GET requestContext
