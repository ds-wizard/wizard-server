module Specs.Service.Mcp.Tool.AuthoringGuideTools.ToolSpec (
  authoringGuideToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.AuthoringGuideTools.GetAuthoringGuide

authoringGuideToolsSpec :: RequestContext -> SpecWith ((), Application)
authoringGuideToolsSpec requestContext = do
  get_authoring_guide requestContext
