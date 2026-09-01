module Shared.Api.Handler.Project.Tag.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Service.Project.Tag.ProjectTagService
import Shared.Util.String (splitOn)

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> "project-tags"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "exclude" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page String))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page String))
list_suggestions_GET mTokenHeader mServerUrl mQuery mExclude mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let excludeTags = maybe [] (splitOn ",") mExclude
        getProjectTagSuggestions mQuery excludeTags (Pageable mPage mSize) (parseSortQuery mSort)
