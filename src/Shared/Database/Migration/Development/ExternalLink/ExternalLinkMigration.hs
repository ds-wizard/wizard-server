module Shared.Database.Migration.Development.ExternalLink.ExternalLinkMigration where

import Shared.Constant.Component
import Shared.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(ExternalLink/ExternalLink) started"
  deleteExternalLinkUsages
  logInfo _CMP_MIGRATION "(ExternalLink/ExternalLink) ended"
