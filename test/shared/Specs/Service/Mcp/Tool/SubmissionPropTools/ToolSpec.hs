module Specs.Service.Mcp.Tool.SubmissionPropTools.ToolSpec (
  submissionPropToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.SubmissionPropTools.SetSubmissionProps

submissionPropToolsSpec :: RequestContext -> SpecWith ((), Application)
submissionPropToolsSpec requestContext = do
  set_submission_props requestContext
