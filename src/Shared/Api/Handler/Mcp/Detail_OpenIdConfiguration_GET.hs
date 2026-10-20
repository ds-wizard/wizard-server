module Shared.Api.Handler.Mcp.Detail_OpenIdConfiguration_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO
import Shared.Api.Resource.Mcp.Metadata.McpMetadataJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_OpenIdConfiguration_GET =
  Header "Host" String
    :> "mcp"
    :> ".well-known"
    :> "openid-configuration"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpMetadataDTO)

detail_openIdConfiguration_GET :: WizardHandlerC s sm r rm => Maybe String -> sm (Headers '[Header "x-trace-uuid" String] McpMetadataDTO)
detail_openIdConfiguration_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< getMcpMetadata
