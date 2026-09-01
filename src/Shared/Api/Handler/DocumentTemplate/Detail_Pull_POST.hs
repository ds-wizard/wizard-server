module Shared.Api.Handler.DocumentTemplate.Detail_Pull_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "id" Coordinate
    :> "pull"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

detail_pull_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Coordinate -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
detail_pull_POST mTokenHeader mServerUrl dtId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry dtId
