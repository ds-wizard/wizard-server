module Shared.Api.Handler.Mcp.Detail_Codes_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Code.McpCodeCreateJM ()
import Shared.Api.Resource.Mcp.Code.McpCodeDTO
import Shared.Api.Resource.Mcp.Code.McpCodeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Codes_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] McpCodeCreateDTO
    :> "mcp"
    :> "oauth2"
    :> "codes"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpCodeDTO)

detail_codes_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> McpCodeCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] McpCodeDTO)
detail_codes_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createMcpCodeForCurrentUser reqDto
