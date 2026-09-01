module Shared.Api.Handler.PersistentCommand.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.PersistentCommand.WizardPersistentCommandService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PersistentCommandChangeDTO
    :> "persistent-commands"
    :> Capture "pcUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> PersistentCommandChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPersistentCommand uuid reqDto
