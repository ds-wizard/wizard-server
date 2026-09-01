module Shared.Database.Migration.Development.UserEmailLink.UserEmailLinkMigration where

import Shared.Constant.Component
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(UserEmailLink/UserEmailLink) started"
  deleteUserEmailLinks
  insertUserEmailLink differentUserEmailLink
  logInfo _CMP_MIGRATION "(UserEmailLink/UserEmailLink) ended"
