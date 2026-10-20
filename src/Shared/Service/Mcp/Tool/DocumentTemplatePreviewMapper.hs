module Shared.Service.Mcp.Tool.DocumentTemplatePreviewMapper where

import qualified Data.ByteString as BS
import Data.List (isInfixOf, isPrefixOf)

import Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewDTO
import Shared.Model.Document.Document
import Shared.Service.Mcp.Tool.McpToolCommon (decodeText)
import Shared.Util.WorkerLog (trimWorkerLog)

toPendingResult :: Document -> McpDocumentPreviewDTO
toPendingResult = emptyResult PendingMcpDocumentPreviewState

toMetadataResult :: Document -> McpDocumentPreviewDTO
toMetadataResult doc =
  (emptyResult DoneMcpDocumentPreviewState doc)
    { fileName = doc.fileName
    , contentType = doc.contentType
    , fileSize = doc.fileSize
    }

toTextResult :: Document -> BS.ByteString -> McpDocumentPreviewDTO
toTextResult doc bytes =
  let text = decodeText bytes
   in (toMetadataResult doc) {content = Just text, contentLength = Just (length text)}

toErrorResult :: Bool -> Document -> McpDocumentPreviewDTO
toErrorResult fullLog doc =
  (emptyResult ErrorMcpDocumentPreviewState doc)
    { errorMessage = fmap trimWorkerLog doc.workerLog
    , workerLog = if fullLog then doc.workerLog else Nothing
    }

emptyResult :: McpDocumentPreviewState -> Document -> McpDocumentPreviewDTO
emptyResult state doc = McpDocumentPreviewDTO state doc.uuid Nothing Nothing Nothing Nothing Nothing Nothing Nothing

previewProgress :: Document -> Maybe String
previewProgress doc =
  case doc.state of
    QueuedDocumentState -> Just "The preview waits for the document worker"
    InProgressDocumentState -> Just "The document worker generates the preview"
    _ -> Nothing

isTextContentType :: Maybe String -> Bool
isTextContentType (Just contentType) = "text/" `isPrefixOf` contentType || any (`isInfixOf` contentType) ["json", "xml"]
isTextContentType Nothing = False
