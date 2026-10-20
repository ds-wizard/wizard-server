module Specs.Service.Mcp.Tool.DocumentTemplatePublishTools.ToolSpec (
  documentTemplatePublishToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplatePublishTools.PublishDocumentTemplateDraft

documentTemplatePublishToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplatePublishToolsSpec requestContext = do
  publish_document_template_draft requestContext
