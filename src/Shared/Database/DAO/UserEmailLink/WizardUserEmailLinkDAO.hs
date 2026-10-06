module Shared.Database.DAO.UserEmailLink.WizardUserEmailLinkDAO where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext

deleteUserEmailLinksExpiredByTenantConfig :: WizardRequestContextC s m => m Int64
deleteUserEmailLinksExpiredByTenantConfig = do
  let sql =
        fromString
          "DELETE FROM user_email_link \
          \USING settings_authentication \
          \WHERE user_email_link.tenant_uuid = settings_authentication.tenant_uuid \
          \  AND user_email_link.created_at < (now() - (settings_authentication.user_email_link_expiration * interval '1 hour'));"
  logQuery sql ()
  let action conn = execute_ conn sql
  runDB action
