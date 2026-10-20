module Specs.Service.Mcp.Tool.DocumentTemplateFileTools.ToolSpec (
  documentTemplateFileToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.DeleteDocumentTemplateFiles
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.ReadDocumentTemplateAssets
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.ReadDocumentTemplateFiles
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.WriteDocumentTemplateAssets
import Specs.Service.Mcp.Tool.DocumentTemplateFileTools.WriteDocumentTemplateFiles

documentTemplateFileToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplateFileToolsSpec requestContext = do
  read_document_template_files requestContext
  write_document_template_files requestContext
  read_document_template_assets requestContext
  write_document_template_assets requestContext
  delete_document_template_files requestContext
