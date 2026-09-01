module Shared.Api.Handler.DocumentTemplate.List_Suggestions_GET where

import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.DocumentTemplatePhaseJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.DocumentTemplateService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> "suggestions"
    :> QueryParam "knowledgeModelPackageUuid" U.UUID
    :> QueryParam "includeUnsupportedMetamodelVersion" Bool
    :> QueryParam "phase" DocumentTemplatePhase
    :> QueryParam "q" String
    :> QueryParam "nonEditable" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSuggestionDTO))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe DocumentTemplatePhase
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSuggestionDTO))
list_suggestions_GET mTokenHeader mServerUrl mPkgUuid mIncludeUnsupportedMetamodelVersion mPhase mQuery mNonEditable mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let includeUnsupportedMetamodelVersion = fromMaybe False mIncludeUnsupportedMetamodelVersion
        getDocumentTemplateSuggestions mPkgUuid includeUnsupportedMetamodelVersion mPhase mQuery mNonEditable (Pageable mPage mSize) (parseSortQuery mSort)
