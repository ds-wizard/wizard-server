module WizardServer.Api.Handler.User.List_Current_Submission_Props_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserSubmissionPropJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserSubmissionPropList
import Shared.Service.User.Profile.UserProfileService

type List_Current_Submission_Props_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] [UserSubmissionPropList]
    :> "users"
    :> "current"
    :> "submission-props"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])

list_current_submission_props_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> [UserSubmissionPropList]
  -> sm (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])
list_current_submission_props_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyUserProfileSubmissionProps reqDto
