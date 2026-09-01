module WizardServer.Api.Handler.Role.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.RoleChangeJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.RoleList
import WizardServer.Service.User.Role.RoleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] RoleChangeDTO
    :> "roles"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] RoleList)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> RoleChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] RoleList)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createRole reqDto
