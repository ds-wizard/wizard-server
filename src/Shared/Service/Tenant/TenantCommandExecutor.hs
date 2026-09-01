module Shared.Service.Tenant.TenantCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Tenant.CreateTenantCommand
import Shared.Service.Tenant.TenantService
import Shared.Util.Logger

cComponent = "tenant"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateTenantName = cCreateTenant command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateTenantName = "createTenant"

cCreateTenant :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cCreateTenant persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateTenantCommand
  case eCommand of
    Right command -> do
      createTenantByCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
