module Shared.Service.Common where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (asks)

import Shared.Localization.Messages.Public
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error

checkIfTenantFeatureIsEnabled :: MonadError AppError m => String -> m config -> (config -> Bool) -> m ()
checkIfTenantFeatureIsEnabled featureName dbFunction accessor = do
  tenantConfig <- dbFunction
  if accessor tenantConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

checkIfServerFeatureIsEnabled :: WizardRequestContextC s m => String -> (ServerConfig -> Bool) -> m ()
checkIfServerFeatureIsEnabled featureName accessor = do
  serverConfig <- asks (.serverConfig')
  if accessor serverConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
