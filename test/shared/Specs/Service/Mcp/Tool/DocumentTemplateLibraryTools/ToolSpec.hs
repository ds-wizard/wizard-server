module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ToolSpec (
  documentTemplateLibraryToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.DeleteDocumentTemplate
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ExportDocumentTemplate
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.GetDocumentTemplate
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ImportDocumentTemplate
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ListDocumentTemplates
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.PullDocumentTemplate
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.UpdateDocumentTemplate

documentTemplateLibraryToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplateLibraryToolsSpec requestContext = do
  list_document_templates requestContext
  get_document_template requestContext
  pull_document_template requestContext
  update_document_template requestContext
  delete_document_template requestContext
  export_document_template requestContext
  import_document_template requestContext
