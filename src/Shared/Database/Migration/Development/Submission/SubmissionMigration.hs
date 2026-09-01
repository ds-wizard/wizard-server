module Shared.Database.Migration.Development.Submission.SubmissionMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.Migration.Development.Submission.Data.Submissions
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Submission/Submission) started"
  deleteTenantConfigSubmissions
  deleteSubmissions
  insertTenantConfigSubmission defaultSubmission
  insertSubmission submission1
  insertTenantConfigSubmission differentSubmission
  insertSubmission differentSubmission1
  logInfo _CMP_MIGRATION "(Submission/Submission) ended"
