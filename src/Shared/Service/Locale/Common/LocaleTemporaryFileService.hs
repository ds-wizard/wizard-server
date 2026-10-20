module Shared.Service.Locale.Common.LocaleTemporaryFileService where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.TemporaryFile
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService

createTemporaryFileWithContent :: WizardRequestContextC s m => String -> String -> BS.ByteString -> m TemporaryFileDTO
createTemporaryFileWithContent fileName contentType content = do
  mCurrentUserUuid <- getCurrentUserUuid
  url <- createTemporaryFile temporaryFileExpiration fileName contentType mCurrentUserUuid (BSL.fromStrict content)
  return $ TemporaryFileMapper.toDTO url contentType
