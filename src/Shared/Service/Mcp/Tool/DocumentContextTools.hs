module Shared.Service.Mcp.Tool.DocumentContextTools where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Text.Read (readMaybe)

import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Model.Error.Error
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.Document.Context.DocumentContextService
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema
import Shared.Util.String (splitOn)

documentContextTools :: WizardRequestContextC s m => [McpTool m]
documentContextTools = [getDocumentContext]

getDocumentContext :: forall s m. WizardRequestContextC s m => McpTool m
getDocumentContext =
  markThirdPartyFields [["value"]] $
    McpTool
      { name = "get_document_context"
      , title = "Get document context"
      , description =
          "Get the document context a template receives (get_authoring_guide topic document-context), built live from a project, optionally as of one of its events, "
            ++ "or from a knowledge model editor with its unpublished changes and preview replies. "
            ++ "path selects a subtree as a JSON Pointer, for example /questionnaire/replies or /knowledgeModel/entities/questions/<uuid>, with list items by their index. "
            ++ "A subtree too large to return is cut; continue with the cursor of the result or read its parts with longer paths"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just documentContextSchema
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just DtMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ \(getContext, path) -> selectSubtree path =<< getContext
      }
  where
    arguments =
      (,)
        <$> contextSourceArguments
        <*> (fromMaybe "" <$> optionalArgument "path" "The JSON Pointer of the subtree to return; the whole context by default")
        :: Arguments (m DocumentContext, String)

contextSourceArguments :: WizardRequestContextC s m => Arguments (m DocumentContext)
contextSourceArguments =
  checkArguments contextSource $
    (,,)
      <$> optionalArgument "project_uuid" "The project whose document context to read"
      <*> optionalArgument "event_uuid" "With project_uuid, the event of the project as of which to read its replies; the latest state by default"
      <*> optionalArgument "knowledge_model_editor_uuid" "The knowledge model editor whose document context to read, as its previews get it"

contextSource :: WizardRequestContextC s m => (Maybe U.UUID, Maybe U.UUID, Maybe U.UUID) -> Either LocaleRecord (m DocumentContext)
contextSource (Just projectUuid, mEventUuid, Nothing) = Right (getProjectDocumentContext projectUuid mEventUuid)
contextSource (Nothing, Nothing, Just editorUuid) = Right (getKnowledgeModelEditorDocumentContext editorUuid)
contextSource (Nothing, Just _, Just _) = Left $ _ERROR_VALIDATION__MCP_DEPENDENT_ARGUMENT "event_uuid" "project_uuid"
contextSource _ = Left $ _ERROR_VALIDATION__MCP_ONE_OF_ARGUMENTS "project_uuid" "knowledge_model_editor_uuid"

selectSubtree :: WizardRequestContextC s m => String -> DocumentContext -> m Value
selectSubtree path ctx =
  case valueAt (splitOn "/" (dropWhile (== '/') path)) (toJSON ctx) of
    Just value -> return $ object ["hash" .= ctx.hash, "path" .= path, "value" .= value]
    Nothing -> throwError . UserError $ _ERROR_SERVICE_MCP__DOCUMENT_CONTEXT_PATH_NOT_FOUND path

valueAt :: [String] -> Value -> Maybe Value
valueAt [] value = Just value
valueAt (key : rest) (Object o) = valueAt rest =<< KM.lookup (K.fromString key) o
valueAt (key : rest) (Array items) = valueAt rest =<< (items V.!?) =<< readMaybe key
valueAt _ _ = Nothing

documentContextSchema :: Value
documentContextSchema =
  objectSchema
    ["hash", "path", "value"]
    [ ("hash", describedSchema "The hash of the whole document context; it changes with its content" stringSchema)
    , ("path", stringSchema)
    , ("value", describedSchema "The subtree at path" (object []))
    ]
