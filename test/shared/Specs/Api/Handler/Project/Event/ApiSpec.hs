module Specs.Api.Handler.Project.Event.ApiSpec where

import Test.Hspec

import Specs.Api.Handler.Project.Event.Detail_GET
import Specs.Api.Handler.Project.Event.List_Batch_POST
import Specs.Api.Handler.Project.Event.List_GET
import Specs.Api.Handler.Project.Event.List_POST

projectEventAPI requestContext =
  describe "PROJECT EVENT API Spec" $ do
    list_GET requestContext
    list_POST requestContext
    list_batch_POST requestContext
    detail_GET requestContext
