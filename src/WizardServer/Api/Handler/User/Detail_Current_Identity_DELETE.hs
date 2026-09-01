module WizardServer.Api.Handler.User.Detail_Current_Identity_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Service.User.ExternalIdentity.UserExternalIdentityService

type Detail_Current_Identity_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "identities"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_current_identity_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_current_identity_DELETE mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteUserIdentity uuid
        return NoContent
