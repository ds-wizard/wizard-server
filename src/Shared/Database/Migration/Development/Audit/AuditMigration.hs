module Shared.Database.Migration.Development.Audit.AuditMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Audit.AuditDAO
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Audit/Audit) started"
  deleteAudits
  logInfo _CMP_MIGRATION "(Audit/Audit) ended"
