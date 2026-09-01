module Specs.Api.Handler.KnowledgeModel.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModel.Preview_POST

knowledgeModelAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "KNOWLEDGE MODEL API Spec" $ preview_POST requestContext
