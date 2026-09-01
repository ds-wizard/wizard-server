module Shared.Api.Handler.DocumentTemplate.List_Bundle_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "document-templates"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

list_bundle_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> FileDTO
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importAndConvertBundle reqDto.content False
