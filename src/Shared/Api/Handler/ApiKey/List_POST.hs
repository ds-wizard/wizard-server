module Shared.Api.Handler.ApiKey.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.ApiKeyCreateJM ()
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Api.Resource.UserToken.UserTokenJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.UserToken.ApiKey.ApiKeyService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] ApiKeyCreateDTO
    :> "api-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> ApiKeyCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mTokenHeader mServerUrl mUserAgent reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createApiKey reqDto mUserAgent
