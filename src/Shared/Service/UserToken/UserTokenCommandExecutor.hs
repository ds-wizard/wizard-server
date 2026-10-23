module Shared.Service.UserToken.UserTokenCommandExecutor where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Model.Cache.ServerCache
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.UserToken.RevokeUserTokensCommand
import Shared.Service.UserToken.UserTokenService
import Shared.Util.JSON (eitherDecodeFromString)
import Shared.Util.Logger

cComponent = "user_token"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cRevokeName = cRevoke command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cRevokeName = "revoke"

cRevoke :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cRevoke persistentCommand = do
  let eCommand = eitherDecodeFromString persistentCommand.body :: Either String RevokeUserTokensCommand
  case eCommand of
    Right command -> asSystemUser $ do
      deleteTokensByUserUuid command.userUuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
