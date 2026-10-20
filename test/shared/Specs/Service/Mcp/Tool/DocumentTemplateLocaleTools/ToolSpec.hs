module Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.ToolSpec (
  documentTemplateLocaleToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.DeleteDocumentTemplateLocale
import Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.GetDocumentTemplateTranslation
import Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.SaveDocumentTemplateLocale

documentTemplateLocaleToolsSpec :: RequestContext -> SpecWith ((), Application)
documentTemplateLocaleToolsSpec requestContext = do
  get_document_template_translation requestContext
  save_document_template_locale requestContext
  delete_document_template_locale requestContext
