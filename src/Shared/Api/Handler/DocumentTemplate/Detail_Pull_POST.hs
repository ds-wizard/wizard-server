module Shared.Api.Handler.DocumentTemplate.Detail_Pull_POST where

import qualified Data.UUID as U
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
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

detail_pull_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Coordinate -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
detail_pull_POST mTokenHeader mServerUrl dtId mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry dtId
