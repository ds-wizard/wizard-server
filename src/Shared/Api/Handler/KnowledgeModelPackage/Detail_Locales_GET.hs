module Shared.Api.Handler.KnowledgeModelPackage.Detail_Locales_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelLocaleList])

detail_locales_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [KnowledgeModelLocaleList])
detail_locales_GET mTokenHeader mServerUrl pkgUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getLocalesForPackage pkgUuid
