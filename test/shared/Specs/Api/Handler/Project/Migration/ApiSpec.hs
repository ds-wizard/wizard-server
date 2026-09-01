module Specs.Api.Handler.Project.Migration.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.Migration.List_POST

projectMigrationAPI requestContext =
  describe "PROJECT MIGRATION API Spec" $
    list_POST requestContext
