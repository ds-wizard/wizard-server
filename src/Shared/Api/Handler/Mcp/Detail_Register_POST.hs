module Shared.Api.Handler.Mcp.Detail_Register_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Mcp.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateJM ()
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Register_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] McpClientRegistrationCreateDTO
    :> "mcp"
    :> "oauth2"
    :> "register"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpClientRegistrationDTO)

detail_register_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> McpClientRegistrationCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] McpClientRegistrationDTO)
detail_register_POST mServerUrl reqDto =
  respondOAuth . runInUnauthService mServerUrl Transactional $
    traverse addTraceUuidHeader =<< registerMcpClient reqDto
