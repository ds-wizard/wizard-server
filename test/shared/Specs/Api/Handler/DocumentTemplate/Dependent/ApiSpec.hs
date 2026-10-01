module Specs.Api.Handler.DocumentTemplate.Dependent.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.DocumentTemplate.Dependent.List_GET

dependentAPI requestContext =
  describe "DOCUMENT TEMPLATE DEPENDENT API Spec" $ do
    list_GET requestContext
