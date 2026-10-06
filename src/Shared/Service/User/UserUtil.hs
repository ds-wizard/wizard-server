module Shared.Service.User.UserUtil where

import Control.Monad.Reader (asks)
import Data.Maybe (isJust, isNothing)

import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.User
import Shared.Model.User.UserSubmissionPropEM ()

isConsentRequired :: WizardRequestContextC s m => Maybe User -> m Bool
isConsentRequired mUserFromDb = do
  serverConfig <- asks (.serverConfig')
  return $ isNothing mUserFromDb && (isJust serverConfig.general.privacyUrl || isJust serverConfig.general.termsOfServiceUrl)
