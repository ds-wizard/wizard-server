module Shared.Database.Mapping.Submission.SubmissionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.Submission.Submission ()
import Shared.Database.Mapping.User.UserSuggestion
import Shared.Model.Submission.SubmissionList

instance FromRow SubmissionList where
  fromRow = do
    uuid <- field
    state <- field
    location <- field
    returnedData <- field
    documentUuid <- field
    createdAt <- field
    updatedAt <- field
    serviceId <- field
    serviceName <- field
    createdBy <- fieldUserSuggestion'
    return $ SubmissionList {..}
