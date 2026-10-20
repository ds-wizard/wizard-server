module Specs.Service.Mcp.Tool.DocumentTemplatePreviewTools.ToolSpec (
  documentTemplatePreviewToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplatePreviewTools.PreviewDocumentTemplate

documentTemplatePreviewToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplatePreviewToolsSpec requestContext = do
  preview_document_template requestContext
