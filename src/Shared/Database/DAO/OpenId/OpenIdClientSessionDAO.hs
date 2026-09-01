module Shared.Database.DAO.OpenId.OpenIdClientSessionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.OpenId.OpenIdClientSession ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.OpenId.OpenIdClientSession

entityName = "openid_client_session"

findOpenIdClientSessionByState' :: WizardRequestContextC s m => String -> m (Maybe OpenIdClientSession)
findOpenIdClientSessionByState' state = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("state", state)]

insertOpenIdClientSession :: WizardRequestContextC s m => OpenIdClientSession -> m Int64
insertOpenIdClientSession = createInsertFn entityName

deleteOpenIdClientSessionByState :: WizardRequestContextC s m => String -> m Int64
deleteOpenIdClientSessionByState state = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("state", state)]

deleteExpiredOpenIdClientSessions :: WizardRequestContextC s m => m Int64
deleteExpiredOpenIdClientSessions = do
  let sql = fromString "DELETE FROM openid_client_session WHERE created_at < now() - interval '1 hour'"
  let action conn = execute_ conn sql
  runDB action
