module Shared.Api.Handler.Token.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Api.Resource.UserToken.LoginJM ()
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Api.Resource.UserToken.UserTokenJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.UserToken.Login.LoginService

type List_POST =
  Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] LoginDTO
    :> "tokens"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> LoginDTO -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mServerUrl mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< createLoginTokenFromCredentials reqDto mUserAgent
