module Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.DocumentTemplate.Asset.DocumentTemplateAssetDeleteFromS3Command
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Util.Logger

cComponent = "document_template_asset"

execute :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
execute command
  | command.function == cDeleteFromS3Name = cDeleteFromS3 command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cDeleteFromS3Name = "deleteFromS3"

cDeleteFromS3 :: WizardRequestContextC s m => PersistentCommand U.UUID -> m (PersistentCommandState, Maybe String)
cDeleteFromS3 persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DocumentTemplateAssetDeleteFromS3Command
  case eCommand of
    Right command -> do
      removeAsset command.documentTemplateUuid command.assetUuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
