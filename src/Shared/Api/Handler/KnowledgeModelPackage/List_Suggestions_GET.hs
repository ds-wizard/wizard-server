module Shared.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "select" [Coordinate]
    :> QueryParam "exclude" [Coordinate]
    :> QueryParam "phase" KnowledgeModelPackagePhase
    :> QueryParam "nonEditable" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSuggestion))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe [Coordinate]
  -> Maybe [Coordinate]
  -> Maybe KnowledgeModelPackagePhase
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        getPackageSuggestions mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable (Pageable mPage mSize) (parseSortQuery mSort)
