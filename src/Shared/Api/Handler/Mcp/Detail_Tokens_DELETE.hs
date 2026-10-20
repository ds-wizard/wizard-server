module Shared.Api.Handler.Mcp.Detail_Tokens_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Tokens_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "mcp"
    :> "tokens"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_tokens_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_tokens_DELETE mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteMcpTokenForCurrentUser uuid
        return NoContent
