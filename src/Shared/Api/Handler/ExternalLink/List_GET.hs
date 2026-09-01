module Shared.Api.Handler.ExternalLink.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Service.ExternalLink.ExternalLinkUsageService

type List_GET =
  Header "Host" String
    :> "external-link"
    :> QueryParam' '[Required] "url" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_GET :: WizardHandlerC s sm r rm => Maybe String -> String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_GET mServerUrl url =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      createExternalLinkUsage url
      throwError $ FoundError url
