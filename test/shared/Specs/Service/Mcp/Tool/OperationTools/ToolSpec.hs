module Specs.Service.Mcp.Tool.OperationTools.ToolSpec (
  operationToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.OperationTools.ExecuteDevOperation
import Specs.Service.Mcp.Tool.OperationTools.GetPersistentCommand
import Specs.Service.Mcp.Tool.OperationTools.ListDevOperations
import Specs.Service.Mcp.Tool.OperationTools.ListPersistentCommands
import Specs.Service.Mcp.Tool.OperationTools.RetryPersistentCommand
import Specs.Service.Mcp.Tool.OperationTools.UpdatePersistentCommand

operationToolsSpec :: RequestContext -> SpecWith ((), Application)
operationToolsSpec requestContext = do
  list_dev_operations requestContext
  execute_dev_operation requestContext
  list_persistent_commands requestContext
  get_persistent_command requestContext
  update_persistent_command requestContext
  retry_persistent_command requestContext
