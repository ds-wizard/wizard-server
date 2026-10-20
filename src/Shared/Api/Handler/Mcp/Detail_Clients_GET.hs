module Shared.Api.Handler.Mcp.Detail_Clients_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Mcp.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Client.McpClientDTO
import Shared.Api.Resource.Mcp.Client.McpClientJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Clients_GET =
  Header "Host" String
    :> "mcp"
    :> "oauth2"
    :> "clients"
    :> Capture "clientId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpClientDTO)

detail_clients_GET :: WizardHandlerC s sm r rm => Maybe String -> String -> sm (Headers '[Header "x-trace-uuid" String] McpClientDTO)
detail_clients_GET mServerUrl clientId =
  respondOAuth . runInUnauthService mServerUrl NoTransaction $
    traverse addTraceUuidHeader =<< getMcpClient clientId
