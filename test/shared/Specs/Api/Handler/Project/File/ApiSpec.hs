module Specs.Api.Handler.Project.File.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.File.Detail_DELETE

projectFileAPI requestContext =
  describe "PROJECT FILE API Spec" $
    detail_DELETE requestContext
