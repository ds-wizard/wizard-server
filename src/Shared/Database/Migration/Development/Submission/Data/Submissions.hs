module Shared.Database.Migration.Development.Submission.Data.Submissions where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Document.Document
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Service.Submission.SubmissionMapper
import Shared.Util.Uuid

submissionCreate :: SubmissionCreateDTO
submissionCreate = SubmissionCreateDTO {serviceId = defaultSubmissionService.sId}

submission1 :: Submission
submission1 =
  Submission
    { uuid = u' "f9e14cfb-4435-45a5-8a10-2ea5dbcb92b0"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , tenantUuid = defaultTenant.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission1List :: SubmissionList
submission1List = toList submission1 defaultSubmissionService (Just userAlbertSuggestion)

submission2 :: Submission
submission2 =
  Submission
    { uuid = u' "bca23893-1d44-4980-a3ba-b50c6b8df342"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , tenantUuid = defaultTenant.uuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission2Dto :: SubmissionList
submission2Dto = toList submission2 defaultSubmissionService (Just userAlbertSuggestion)

differentSubmission1 :: Submission
differentSubmission1 =
  Submission
    { uuid = u' "de51c280-2a6f-49d0-b3de-405401ffba74"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = differentDoc.uuid
    , tenantUuid = differentTenant.uuid
    , createdBy = Just userCharles.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
