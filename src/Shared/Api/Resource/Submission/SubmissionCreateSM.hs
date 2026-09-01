module Shared.Api.Resource.Submission.SubmissionCreateSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Api.Resource.Submission.SubmissionCreateJM ()
import Shared.Database.Migration.Development.Submission.Data.Submissions
import Shared.Util.Swagger

instance ToSchema SubmissionCreateDTO where
  declareNamedSchema = toSwagger submissionCreate
