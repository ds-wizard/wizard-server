module Shared.Service.DocumentTemplate.Locale.Pot.PotFileService where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.TemporaryFile
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.DocumentTemplate.DocumentTemplateGeneratePotFileCommand
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Service.DocumentTemplate.DocumentTemplateAcl
import Shared.Service.PersistentCommand.PersistentCommandMapper
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Util.JSON
import Shared.Util.Uuid

cComponent = "doc_worker"

cGeneratePotFileName = "generatePotFile"

getTemporaryFileWithPotFile :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getTemporaryFileWithPotFile dtUuid =
  runInTransaction $ do
    (dt, content) <- getPotFile dtUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile temporaryFileExpiration (potFileName dt.id dt.version) "application/octet-stream" mCurrentUserUuid (BSL.fromStrict content)
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

getPotFile :: WizardRequestContextC s m => U.UUID -> m (DocumentTemplate, BS.ByteString)
getPotFile dtUuid = do
  checkManagePermissionToDocumentTemplate dtUuid
  dt <- findDocumentTemplateByUuid dtUuid
  unless dt.potFileReady (throwError . UserError $ _ERROR_SERVICE_DOC_TML__POT_FILE_NOT_READY)
  content <- retrievePotFile dt.uuid (potFileName dt.id dt.version)
  return (dt, content)

publishGeneratePotFileCommand :: WizardRequestContextC s m => DocumentTemplate -> m ()
publishGeneratePotFileCommand dt = do
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let body =
        DocumentTemplateGeneratePotFileCommand
          { documentTemplateUuid = dt.uuid
          , id = dt.id
          , version = dt.version
          , language = dt.language
          }
  mCurrentUserUuid <- getCurrentUserUuid
  let command = toPersistentCommand uuid cComponent cGeneratePotFileName (encodeJsonToString body) 10 dt.tenantUuid mCurrentUserUuid now
  void $ insertPersistentCommand command
