module Shared.Service.Mcp.Tool.OperationTools where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Aeson
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Api.Resource.Dev.DevExecutionResultJM ()
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Api.Resource.Dev.DevSectionJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandListJM ()
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Dev.Dev
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Model.User.RolePermission
import Shared.Service.Dev.DevOperationService
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpExecutableServices
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon (idempotentWriteAnnotations, writeAnnotations)
import Shared.Util.JsonSchema

operationTools :: (WizardRequestContextC s m, ToJSON command, JsonSchema command) => McpOperationServices m command -> [McpTool m]
operationTools services =
  [ listDevOperationsTool services
  , executeDevOperationTool services
  , listPersistentCommandsTool services
  , getPersistentCommandTool services
  , updatePersistentCommandTool services
  , retryPersistentCommandTool services
  ]

listDevOperationsTool :: WizardRequestContextC s m => McpOperationServices m command -> McpTool m
listDevOperationsTool services =
  McpTool
    { name = "list_dev_operations"
    , title = "List dev operations"
    , description = "List the dev operations of the instance by section, each with the names and types of its parameters; execute_dev_operation runs one"
    , inputSchema = argumentsSchema (pure ())
    , outputSchema = Just $ objectSchema ["sections"] [("sections", arraySchema (jsonSchema (Proxy :: Proxy DevSectionDTO)))]
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = operationReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments (pure ()) . const $ (\dtos -> object ["sections" .= dtos]) <$> (getDevOperations =<< services.devSections)
    }

executeDevOperationTool :: WizardRequestContextC s m => McpOperationServices m command -> McpTool m
executeDevOperationTool services =
  McpTool
    { name = "execute_dev_operation"
    , title = "Execute dev operation"
    , description =
        "Run a dev operation (list_dev_operations lists them) with its parameters given by name, all values as strings. "
          ++ "Dev operations act on the whole instance and cannot be undone. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ confirmationOutputSchema (jsonSchema (Proxy :: Proxy AdminExecutionResultDTO))
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = operationWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "execute_dev_operation" arguments (prepareDevOperation services)
    }
  where
    arguments =
      (,,)
        <$> requiredArgument "section" "The name of the section"
        <*> requiredArgument "operation" "The name of the operation"
        <*> optionalArgument "parameters" "The values of the parameters of the operation by their names"
        :: Arguments (String, String, Maybe (M.Map String String))

prepareDevOperation :: WizardRequestContextC s m => McpOperationServices m command -> (String, String, Maybe (M.Map String String)) -> m (McpConfirmation m)
prepareDevOperation services (sectionName, operationName, mParameters) = do
  sections <- services.devSections
  operation <- maybe (throwError . UserError $ _ERROR_SERVICE_MCP__DEV_OPERATION_ABSENCE sectionName operationName) return (findDevOperation sections sectionName operationName)
  let parameters = fromMaybe M.empty mParameters
  let missing = filter (`M.notMember` parameters) (fmap (.name) operation.parameters)
  unless (null missing) (throwError . UserError $ _ERROR_SERVICE_MCP__DEV_OPERATION_PARAMETERS operationName missing)
  let values = fmap (\parameter -> M.findWithDefault "" parameter.name parameters) operation.parameters
  return
    McpConfirmation
      { summary = "Runs the dev operation '" ++ operationName ++ "' of the section '" ++ sectionName ++ "'" ++ parametersSummary operation.parameters values ++ "; it cannot be undone"
      , revision = ""
      , perform = toJSON <$> executeOperation sections (DevExecutionDTO sectionName operationName values)
      }

findDevOperation :: [DevSection m] -> String -> String -> Maybe (DevOperation m)
findDevOperation sections sectionName operationName = do
  section <- find ((== sectionName) . (.name)) sections
  find ((== operationName) . (.name)) section.operations

parametersSummary :: [DevOperationParameter] -> [String] -> String
parametersSummary [] _ = ""
parametersSummary parameters values = " with " ++ intercalate ", " (zipWith (\parameter value -> parameter.name ++ " = " ++ value) parameters values)

listPersistentCommandsTool :: WizardRequestContextC s m => McpOperationServices m command -> McpTool m
listPersistentCommandsTool services =
  McpTool
    { name = "list_persistent_commands"
    , title = "List persistent commands"
    , description = "List the persistent commands of the instance, the newest first, optionally only those in some states; retry_persistent_command runs a failed one again"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ pageSchema "persistentCommands" (jsonSchema (Proxy :: Proxy PersistentCommandList))
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = operationReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \(mStates, pageable) ->
        toJSON <$> services.getPersistentCommandsPage (maybe [] (fmap show) mStates) pageable [Sort "createdAt" Descending]
    }
  where
    arguments =
      (,) <$> optionalArgument "states" "Only the commands in one of these states" <*> pageableArguments
        :: Arguments (Maybe [PersistentCommandState], Pageable)

getPersistentCommandTool :: (WizardRequestContextC s m, ToJSON command) => McpOperationServices m command -> McpTool m
getPersistentCommandTool services =
  McpTool
    { name = "get_persistent_command"
    , title = "Get persistent command"
    , description = "Get a persistent command with its body, which may hold links and other secrets, and its last error"
    , inputSchema = argumentsSchema commandUuidArgument
    , outputSchema = Just $ describedSchema "The command, shaped as retry_persistent_command returns it" (typeSchema "object")
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = _MCP_SENSITIVE_ROLE_PERMISSION : operationReadPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = NoTransaction
    , run = withArguments commandUuidArgument $ fmap toJSON . services.getPersistentCommandById
    }

updatePersistentCommandTool :: (WizardRequestContextC s m, ToJSON command) => McpOperationServices m command -> McpTool m
updatePersistentCommandTool services =
  McpTool
    { name = "update_persistent_command"
    , title = "Update persistent command"
    , description = "Set the state of a persistent command, for example IgnorePersistentCommandState so that it is no longer retried, and get the command afterwards"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ describedSchema "The command, shaped as retry_persistent_command returns it" (typeSchema "object")
    , annotations = idempotentWriteAnnotations
    , continuable = False
    , permissions = _MCP_SENSITIVE_ROLE_PERMISSION : operationWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(uuid, state) -> toJSON <$> services.modifyPersistentCommand uuid (PersistentCommandChangeDTO state)
    }
  where
    arguments =
      (,) <$> commandUuidArgument <*> requiredArgument "state" "The new state of the command"
        :: Arguments (U.UUID, PersistentCommandState)

retryPersistentCommandTool :: forall s m command. (WizardRequestContextC s m, ToJSON command, JsonSchema command) => McpOperationServices m command -> McpTool m
retryPersistentCommandTool services =
  McpTool
    { name = "retry_persistent_command"
    , title = "Retry persistent command"
    , description =
        "Run a new, failed or ignored persistent command again and get it with its last error; "
          ++ "a command of an external component (such as the mailer) is handed back to its queue and runs there"
    , inputSchema = argumentsSchema commandUuidArgument
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy command)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = _MCP_SENSITIVE_ROLE_PERMISSION : operationWritePermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments commandUuidArgument $ fmap toJSON . retryPersistentCommand services
    }

retryPersistentCommand :: WizardRequestContextC s m => McpOperationServices m command -> U.UUID -> m command
retryPersistentCommand services uuid = do
  persistentCommand <- findPersistentCommandByUuid uuid
  when ((persistentCommand :: PersistentCommand U.UUID).state == DonePersistentCommandState) (throwError . UserError $ _ERROR_SERVICE_MCP__PERSISTENT_COMMAND_DONE (U.toString uuid))
  services.runPersistentCommandById uuid

commandUuidArgument :: Arguments U.UUID
commandUuidArgument = requiredArgument "persistent_command_uuid" "The persistent command"

operationReadPermissions :: [String]
operationReadPermissions = [_MCP_READ_ROLE_PERMISSION, _DEV_USE_ROLE_PERMISSION]

operationWritePermissions :: [String]
operationWritePermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _DEV_USE_ROLE_PERMISSION]
