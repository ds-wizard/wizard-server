module Shared.Api.Handler.DocumentTemplate.Dependent.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Library.LibraryDependents
import Shared.Service.DocumentTemplate.DocumentTemplateService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> "dependents"
    :> QueryParam "allVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LibraryDependents)

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] LibraryDependents)
list_GET mTokenHeader mServerUrl uuid mAllVersions =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getDocumentTemplateDependents uuid mAllVersions
