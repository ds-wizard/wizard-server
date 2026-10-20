module Specs.Service.Mcp.Tool.DocumentContextTools.ToolSpec (
  documentContextToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentContextTools.GetDocumentContext

documentContextToolsSpec :: RequestContext -> SpecWith ((), Application)
documentContextToolsSpec requestContext = do
  get_document_context requestContext
