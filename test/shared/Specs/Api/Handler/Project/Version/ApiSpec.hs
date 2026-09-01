module Specs.Api.Handler.Project.Version.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.Version.Detail_DELETE
import Specs.Api.Handler.Project.Version.Detail_PUT
import Specs.Api.Handler.Project.Version.List_GET
import Specs.Api.Handler.Project.Version.List_POST

projectVersionAPI requestContext =
  describe "PROJECT VERSION API Spec" $ do
    list_GET requestContext
    list_POST requestContext
    detail_PUT requestContext
    detail_DELETE requestContext
