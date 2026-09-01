module WizardServer.Service.Config.Server.ServerConfigValidation where

import Shared.Model.Config.WizardServerConfig
import Shared.Model.Config.WizardServerConfigJM ()
import Shared.Model.Error.Error
import Shared.Service.Config.Server.ServerConfigValidation
import WizardServer.Model.Config.ServerConfigIM ()

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralServerPort config
  validateDatabaseMaxConnections config
  validateGeneralSecret config
  validateGeneralRsaPrivateKey config
