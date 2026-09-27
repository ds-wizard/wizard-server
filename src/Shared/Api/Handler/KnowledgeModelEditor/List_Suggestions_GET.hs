module Shared.Api.Handler.KnowledgeModelEditor.List_Suggestions_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Service.KnowledgeModel.Editor.EditorService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> "suggestions"
    :> QueryParam "w" U.UUID
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelEditorSuggestion))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelEditorSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mW mQuery mPage mSize mSort =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW Nothing $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getEditorSuggestionsPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
