module Shared.Database.Migration.Development.Submission.SubmissionMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Submission.Data.Submissions
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Tenant
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Submission/Submission) started"
  deleteSubmissions
  saveSettingsSubmission defaultTenant.uuid Nothing settingsSubmission
  insertSubmission submission1
  saveSettingsSubmission differentTenant.uuid Nothing settingsSubmission
  insertSubmission differentSubmission1
  logInfo _CMP_MIGRATION "(Submission/Submission) ended"
