module WizardServer.Api.Handler.User.List_Current_Submission_Props_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Api.Resource.User.UserSubmissionPropJM ()
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserSubmissionPropList
import Shared.Service.User.Profile.UserProfileService

type List_Current_Submission_Props_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "submission-props"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])

list_current_submission_props_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])
list_current_submission_props_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        getUserProfileSubmissionProps user.uuid
