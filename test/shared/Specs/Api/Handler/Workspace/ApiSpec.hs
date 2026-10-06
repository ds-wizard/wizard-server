module Specs.Api.Handler.Workspace.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Detail_DELETE
import Specs.Api.Handler.Workspace.Detail_GET
import Specs.Api.Handler.Workspace.Detail_Members_DELETE
import Specs.Api.Handler.Workspace.Detail_Members_GET
import Specs.Api.Handler.Workspace.Detail_Members_PUT
import Specs.Api.Handler.Workspace.Detail_PUT
import Specs.Api.Handler.Workspace.List_GET
import Specs.Api.Handler.Workspace.List_POST

workspaceAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "WORKSPACE API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_members_GET requestContext
      detail_members_PUT requestContext
      detail_members_DELETE requestContext
