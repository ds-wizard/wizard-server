module Shared.Api.Handler.PersistentCommand.Detail_Attempts_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.PersistentCommand.WizardPersistentCommandService

type Detail_Attempts_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> Capture "pcUuid" U.UUID
    :> "attempts"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)

detail_attempts_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)
detail_attempts_POST mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< runPersistentCommandById uuid
