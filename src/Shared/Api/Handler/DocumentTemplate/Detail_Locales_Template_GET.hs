module Shared.Api.Handler.DocumentTemplate.Detail_Locales_Template_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.Locale.Pot.PotFileService

type Detail_Locales_Template_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> "template"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_locales_template_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_locales_template_GET mTokenHeader mServerUrl dtUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTemporaryFileWithPotFile dtUuid
