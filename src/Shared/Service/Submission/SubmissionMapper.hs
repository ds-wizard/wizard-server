module Shared.Service.Submission.SubmissionMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.UserSuggestion

toList :: Submission -> TenantConfigSubmissionService -> Maybe UserSuggestion -> SubmissionList
toList Submission {..} service createdBy2 =
  let serviceName = Just service.name
      createdBy = createdBy2
   in SubmissionList {..}

fromCreate :: U.UUID -> String -> U.UUID -> U.UUID -> Maybe U.UUID -> UTCTime -> Submission
fromCreate uuid serviceId documentUuid tenantUuid createdBy now =
  Submission
    { uuid = uuid
    , state = InProgressSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = serviceId
    , documentUuid = documentUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    , updatedAt = now
    }
