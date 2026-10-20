module Shared.Api.Handler.Mcp.Detail_Keys_GET where

import Jose.Jwk
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Keys_GET =
  Header "Host" String
    :> "mcp"
    :> "oauth2"
    :> "keys"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] JwkSet)

detail_keys_GET :: WizardHandlerC s sm r rm => Maybe String -> sm (Headers '[Header "x-trace-uuid" String] JwkSet)
detail_keys_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< getMcpKeys
