module Shared.Api.Handler.KnowledgeModelPackage.List_POST where

import Servant

import qualified Data.ByteString.Lazy.Char8 as BSL
import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[JSONPlain] String
    :> "knowledge-model-packages"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_POST mTokenHeader mServerUrl reqBody =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importAndConvertBundle (BSL.pack reqBody) False
