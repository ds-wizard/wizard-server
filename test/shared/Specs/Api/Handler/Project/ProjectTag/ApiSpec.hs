module Specs.Api.Handler.Project.ProjectTag.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.ProjectTag.List_Suggestions_GET

projectTagAPI requestContext =
  describe "PROJECT TAG API Spec" $
    do list_suggestions_GET requestContext
