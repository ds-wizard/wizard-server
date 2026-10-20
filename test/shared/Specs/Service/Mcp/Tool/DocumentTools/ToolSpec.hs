module Specs.Service.Mcp.Tool.DocumentTools.ToolSpec (
  documentToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.DocumentTools.CreateDocument
import Specs.Service.Mcp.Tool.DocumentTools.DeleteDocument
import Specs.Service.Mcp.Tool.DocumentTools.DownloadDocument
import Specs.Service.Mcp.Tool.DocumentTools.ListDocumentSubmissionServices
import Specs.Service.Mcp.Tool.DocumentTools.ListDocuments
import Specs.Service.Mcp.Tool.DocumentTools.SubmitDocument

documentToolsSpec :: RequestContext -> SpecWith ((), Application)
documentToolsSpec requestContext = do
  list_documents requestContext
  create_document requestContext
  download_document requestContext
  delete_document requestContext
  list_document_submission_services requestContext
  submit_document requestContext
