module WizardServer.Api.Handler.UserEmailLink.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkTypeJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.UserEmailLink.UserEmailLinkType
import Shared.Service.User.UserService

type List_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] (UserEmailLinkDTO UserEmailLinkType)
    :> "user-email-links"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: WizardHandlerC s sm r rm => Maybe String -> UserEmailLinkDTO UserEmailLinkType -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      resetUserPassword reqDto
      return NoContent
