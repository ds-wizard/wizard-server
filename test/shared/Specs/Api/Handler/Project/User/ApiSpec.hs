module Specs.Api.Handler.Project.User.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.User.List_Suggestions_GET

projectUserAPI requestContext =
  describe "PROJECT USER API Spec" $
    list_suggestions_GET requestContext
