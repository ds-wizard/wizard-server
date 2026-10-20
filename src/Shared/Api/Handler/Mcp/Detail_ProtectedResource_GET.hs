module Shared.Api.Handler.Mcp.Detail_ProtectedResource_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_ProtectedResource_GET =
  Header "Host" String
    :> "mcp"
    :> ".well-known"
    :> "oauth-protected-resource"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpProtectedResourceDTO)

detail_protectedResource_GET :: WizardHandlerC s sm r rm => Maybe String -> sm (Headers '[Header "x-trace-uuid" String] McpProtectedResourceDTO)
detail_protectedResource_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< getMcpProtectedResource
