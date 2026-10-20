module Shared.Service.Mcp.Server.McpToolService where

import qualified Control.Exception.Base as E
import Control.Monad (filterM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, liftIO, local)
import Data.Aeson
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Pool (withResource)
import qualified Data.UUID as U

import Shared.Database.DAO.Common (rollbackOpenTransaction)
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Localization.Locale
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.AuditLog.AuditLogSource
import Shared.Model.Context.AclContext (AclContext (..))
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpCaller
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpCursor
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.Server.McpOutputLimit
import Shared.Util.Error (tryError)
import Shared.Util.JSON (encodeJsonToString)
import Shared.Util.JsonSchema (withProperties)
import Shared.Util.Logger

findMcpTool :: [McpTool m] -> String -> Maybe (McpTool m)
findMcpTool tools name = find ((== name) . (.name)) tools

findMcpCaller :: WizardRequestContextC s m => Maybe U.UUID -> Maybe [McpToolset] -> m McpCaller
findMcpCaller mTokenUuid mRequested = do
  mGrant <- maybe (return Nothing) findMcpRefreshTokenByUserTokenUuid' mTokenUuid
  let granted = maybe allMcpToolsets (.toolsets) mGrant
  return McpCaller {tokenUuid = mTokenUuid, grant = mGrant, toolsets = maybe granted (filter (`elem` granted)) mRequested}

withMcpAuditLogSource :: WizardRequestContextC s m => McpCaller -> String -> m a -> m a
withMcpAuditLogSource caller toolName action = do
  mClient <- maybe (return Nothing) (findMcpClientByClientId' . (.clientId)) caller.grant
  let mClientName = fmap (\client -> fromMaybe client.clientId client.clientName) mClient
  local (setAuditLogSource (mcpAuditLogSource caller.tokenUuid mClientName toolName)) action

toolAccess :: WizardRequestContextC s m => [McpToolset] -> McpTool m -> m (Either Value ())
toolAccess toolsets tool
  | Just toolset <- tool.toolset, toolset `notElem` toolsets = return . Left $ toDisabledToolsetError toolset
  | otherwise = maybe (Right ()) (Left . toMissingPermissionError) . listToMaybe <$> findMissingPermissions tool

findMissingPermissions :: WizardRequestContextC s m => McpTool m -> m [String]
findMissingPermissions tool = filterM (fmap (not . or) . traverse hasPermission . permissionAlternatives) tool.permissions

toToolDTO :: McpTool m -> Value
toToolDTO tool =
  object $
    [ "name" .= tool.name
    , "title" .= tool.title
    , "description" .= tool.description
    , "inputSchema" .= withProperties [cursorProperty | tool.continuable] tool.inputSchema
    , "annotations" .= toAnnotationsDTO tool.annotations
    ]
      ++ maybe [] (\schema -> ["outputSchema" .= withProperties (truncatedProperty : [cursorProperty | tool.continuable]) schema]) tool.outputSchema

toAnnotationsDTO :: McpToolAnnotations -> Value
toAnnotationsDTO annotations =
  object
    [ "readOnlyHint" .= annotations.readOnlyHint
    , "destructiveHint" .= annotations.destructiveHint
    , "idempotentHint" .= annotations.idempotentHint
    , "openWorldHint" .= annotations.openWorldHint
    ]

parseToolCall :: McpTool m -> Value -> Either String (Value -> Either String Value, m Value)
parseToolCall tool arguments = do
  (mCursor, toolArguments) <- splitCursorArgument arguments
  when (isJust mCursor && not tool.continuable) (Left (toLocaleMessage _ERROR_VALIDATION__MCP_WRITE_TOOL_CURSOR))
  action <- tool.run toolArguments
  return (limitToolOutput tool mCursor, action)

limitToolOutput :: McpTool m -> Maybe McpCursor -> Value -> Either String Value
limitToolOutput tool mCursor
  | tool.continuable = limitOutput mCursor
  | otherwise = fmap withoutCursor . limitOutput Nothing

runMcpTool :: WizardRequestContextC s m => TransactionState -> (Value -> Either String Value) -> m Value -> m Value
runMcpTool transactionState limit action = do
  context <- ask
  eResult <- liftIO $ runWithToolConnection transactionState context (runRequestContextWithRequestContext (tryError action))
  foldContextResult
    (fmap toToolError . handleToolException)
    (fmap toToolError . internalToolError)
    (fmap toToolError . toErrorMessage)
    (return . either toToolError toToolResult . limit)
    eResult

type ContextResult a = Either E.SomeException (Either String (Either AppError a))

foldContextResult :: (E.SomeException -> r) -> (String -> r) -> (AppError -> r) -> (a -> r) -> ContextResult a -> r
foldContextResult onException _ _ _ (Left exception) = onException exception
foldContextResult _ onMessage _ _ (Right (Left message)) = onMessage message
foldContextResult _ _ onError _ (Right (Right (Left error))) = onError error
foldContextResult _ _ _ onResult (Right (Right (Right result))) = onResult result

rethrowContextResult :: WizardRequestContextC s m => ContextResult a -> m a
rethrowContextResult = foldContextResult (liftIO . E.throwIO) (throwError . GeneralServerError) throwError return

runWithToolConnection :: WizardRequestContextType context => TransactionState -> context -> (context -> IO a) -> IO (Either E.SomeException a)
runWithToolConnection NoTransaction context callback = E.try (callback context)
runWithToolConnection Transactional context callback =
  withResource context.dbPool' $ \connection -> do
    result <- E.try (callback (setDbConnection (Just connection) context))
    rollbackOpenTransaction connection
    return result

handleToolException :: WizardRequestContextC s m => E.SomeException -> m String
handleToolException exception =
  case E.fromException exception of
    Just (asyncException :: E.SomeAsyncException) -> liftIO $ E.throwIO asyncException
    Nothing -> internalToolError (show exception)

toErrorMessage :: WizardRequestContextC s m => AppError -> m String
toErrorMessage (ValidationError formErrors fieldErrors) =
  return . intercalate "; " $ fmap toLocaleMessage formErrors ++ [field ++ ": " ++ toLocaleMessage record | (field, records) <- M.toList fieldErrors, record <- records]
toErrorMessage (UserError record) = return $ toLocaleMessage record
toErrorMessage (SystemLogError record) = return $ toLocaleMessage record
toErrorMessage (UnauthorizedError record) = return $ toLocaleMessage record
toErrorMessage (ForbiddenError record) = return $ toLocaleMessage record
toErrorMessage (NotExistsError record) = return $ toLocaleMessage record
toErrorMessage (ConflictError record payload) = return . toLocaleMessage $ _ERROR_SERVICE_MCP__CONFLICT (toLocaleMessage record) (encodeJsonToString payload)
toErrorMessage error = internalToolError (show error)

internalToolError :: WizardRequestContextC s m => String -> m String
internalToolError detail = do
  logErrorI _CMP_SERVICE ("MCP tool failed: " ++ detail)
  return (toLocaleMessage _ERROR_SERVICE_MCP__INTERNAL_ERROR)

toDisabledToolsetError :: McpToolset -> Value
toDisabledToolsetError toolset = toToolError . toLocaleMessage $ _ERROR_SERVICE_MCP__TOOLSET_NOT_ENABLED (mcpToolsetName toolset)

toMissingPermissionError :: String -> Value
toMissingPermissionError permission = toToolError . toLocaleMessage $ _ERROR_VALIDATION__MISSING_PERMISSION permission
