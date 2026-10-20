module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.ToolSpec (
  documentTemplateDraftToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.DeleteDocumentTemplateDraft
import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.GetDocumentTemplateDraft
import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.ListDocumentTemplateDrafts
import Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.SaveDocumentTemplateDraft

documentTemplateDraftToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplateDraftToolsSpec requestContext = do
  list_document_template_drafts requestContext
  get_document_template_draft requestContext
  save_document_template_draft requestContext
  delete_document_template_draft requestContext
