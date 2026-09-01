module Shared.Api.Handler.KnowledgeModelPackage.List_From_Editor_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishService

type List_From_Editor_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackagePublishEditorDTO
    :> "knowledge-model-packages"
    :> "from-editor"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_from_editor_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> PackagePublishEditorDTO
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_from_editor_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackageFromKnowledgeModelEditor reqDto
