module Shared.Api.Handler.KnowledgeModelPackage.List_POST where

import qualified Data.UUID as U
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
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> String
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_POST mTokenHeader mServerUrl reqBody mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importAndConvertBundle (BSL.pack reqBody) False
