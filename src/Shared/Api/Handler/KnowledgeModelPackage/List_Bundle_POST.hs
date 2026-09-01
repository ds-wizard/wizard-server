module Shared.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleFileJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Service.Owl.OwlService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem KnowledgeModelBundleFile
    :> "knowledge-model-packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_bundle_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelBundleFile
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< if L.isSuffixOf ".ttl" reqDto.fileName || L.isSuffixOf ".owl" reqDto.fileName
          then importOwl reqDto
          else importAndConvertBundle reqDto.content False
