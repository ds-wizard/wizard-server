module WizardServer.Api.Handler.Common where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, asks, liftIO, local)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Tenant
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.UserToken.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Service.Sentry.SentryService
import Shared.Service.User.WizardUserMapper
import Shared.Service.Workspace.WorkspaceScopeService
import WizardServer.Model.Context.ContextMappers
import WizardServer.Model.Context.RequestContext
import WizardServer.Model.Context.ServerContext

instance WizardHandlerC ServerContext ServerContextM RequestContext RequestContextM where
  runIn mServerUrl mUser mW mTenant transactionState function = do
    tenant <- getCurrentTenant mServerUrl
    if tenant.enabled
      then do
        serverContext <- ask
        eResult <-
          liftIO $ requestContextFromServerContext tenant.uuid mUser transactionState serverContext $ \requestContext -> do
            let tenantContext = requestContext {tenantMultiWorkspace = tenant.multiWorkspace}
            guardRequestContext tenantContext (runMonads (withScope mW mTenant function).runRequestContextM tenantContext)
        case eResult of
          Right result -> return result
          Left error -> throwError =<< sendError error
      else throwError =<< sendError (NotExistsError (_ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE (fromMaybe "not-provided" mServerUrl)))
  getScopedAuthServiceExecutor (Just token) mServerUrl mW mTenant callback = do
    userTokenClaims <- validateJwtToken token
    isTokenExistsInDb userTokenClaims mServerUrl
    user <- getCurrentUser userTokenClaims mServerUrl
    if user.active
      then callback (runInScopedAuthService mServerUrl user mW mTenant)
      else throwError =<< (sendError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED)
  getScopedAuthServiceExecutor Nothing _ _ _ _ = throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

withScope :: Maybe U.UUID -> Maybe Bool -> RequestContextM a -> RequestContextM a
withScope mW mTenant function = do
  scope <- resolveScope mW mTenant
  mCurrentUser <- asks (.currentUser)
  workspaceRoles <- maybe (return M.empty) (findWorkspaceRolesByUserUuid . (.uuid)) mCurrentUser
  local (\context -> context {scope = scope, workspaceRoles = workspaceRoles}) function

runWithSystemUser :: RequestContextM a -> ServerContextM a
runWithSystemUser function = do
  serverContext <- ask
  eResult <-
    liftIO $ requestContextFromServerContext defaultTenantUuid (Just . toDTO $ userSystem) NoTransaction serverContext $ \requestContext ->
      guardRequestContext requestContext (runMonads function.runRequestContextM requestContext)
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getCurrentTenant :: Maybe String -> ServerContextM Tenant
getCurrentTenant mServerUrl = do
  serverContext <- ask
  if serverContext.serverConfig'.cloud.enabled
    then case mServerUrl of
      Nothing -> runWithSystemUser . throwError $ NotExistsError (_ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE "not-provided")
      Just serverUrl -> runWithSystemUser $ catchError (findTenantByServerDomain serverUrl) (handleError serverUrl)
    else runWithSystemUser $ catchError (findTenantByUuid defaultTenantUuid) (handleError (U.toString defaultTenantUuid))
  where
    handleError host (NotExistsError _) = throwError $ NotExistsError (_ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE host)
    handleError host error = throwError error
