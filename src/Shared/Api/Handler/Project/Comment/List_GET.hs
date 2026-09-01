module Shared.Api.Handler.Project.Comment.List_GET where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Service.Project.Comment.ProjectCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> QueryParam "path" String
    :> QueryParam "resolved" Bool
    :> "comments"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (M.Map String [ProjectCommentThreadList]))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] (M.Map String [ProjectCommentThreadList]))
list_GET mTokenHeader mServerUrl uuid mPath mResolved =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectCommentsByProjectUuid uuid mPath mResolved
