module Shared.Api.Handler.ProjectCommentThread.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Service.Project.Comment.ProjectCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-comment-threads"
    :> QueryParam "q" String
    :> QueryParam "projectUuid" U.UUID
    :> QueryParam "resolved" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectCommentThreadAssigned))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page ProjectCommentThreadAssigned))
list_GET mTokenHeader mServerUrl mQuery mProjectUuid resolved mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getProjectCommentThreadsPage mQuery mProjectUuid resolved (Pageable mPage mSize) (parseSortQuery mSort)
