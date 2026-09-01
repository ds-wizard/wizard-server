module Shared.Service.Submission.SubmissionAcl where

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Service.Document.DocumentAcl

checkViewPermissionToSubmission :: WizardRequestContextC s m => Document -> m ()
checkViewPermissionToSubmission doc = do
  checkViewPermissionToDoc doc.projectUuid

checkEditPermissionToSubmission :: WizardRequestContextC s m => Document -> m ()
checkEditPermissionToSubmission doc = do
  checkEditPermissionToDoc doc.projectUuid
