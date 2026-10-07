module Shared.Service.Locale.Common.LocaleUploadService where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust, maybeToList)

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Error.Error
import Shared.Util.Gettext
import Shared.Util.LocaleCode (isValidLocaleCode)

data LocalePo = LocalePo
  { header :: PoHeaderInfo
  , entries :: [PoEntry]
  , content :: BS.ByteString
  }

maxLocalePoSize :: Int
maxLocalePoSize = 5 * 1024 * 1024

uiLocaleKinds :: [String]
uiLocaleKinds = ["dsw-client", "fw-client"]

uploadLocalePo
  :: MonadError AppError m
  => Maybe [String]
  -> (String -> m (Maybe e))
  -> (e -> LocaleUploadWarningDTO)
  -> (LocalePo -> Maybe e -> m a)
  -> LocaleUploadDTO
  -> m (LocaleUploadResultDTO a)
uploadLocalePo mAllowedKinds findExisting toWarning store reqDto = do
  po <- parseLocalePo mAllowedKinds reqDto.content
  mExisting <- findExisting po.header.code
  if reqDto.confirmed
    then StoredLocaleUploadResultDTO (isJust mExisting) <$> store po mExisting
    else return $ toPreview po.header (fmap toWarning (maybeToList mExisting))

parseLocalePo :: MonadError AppError m => Maybe [String] -> BS.ByteString -> m LocalePo
parseLocalePo mAllowedKinds content = do
  when (BS.length content > maxLocalePoSize) (throwError . UserError $ _ERROR_VALIDATION__LOCALE_PO_TOO_LARGE)
  entries <- either (throwError . UserError . _ERROR_VALIDATION__LOCALE_PO_UNPARSABLE) return (parsePo content)
  header <- maybe (throwError . UserError $ _ERROR_VALIDATION__LOCALE_PO_MISSING_LANGUAGE) return (poHeaderInfo . poHeaderFields $ entries)
  unless (isValidLocaleCode header.code) (throwError . UserError . _ERROR_VALIDATION__LOCALE_PO_INVALID_LANGUAGE $ header.code)
  forM_ mAllowedKinds (checkKind header)
  return LocalePo {header = header, entries = entries, content = content}

checkKind :: MonadError AppError m => PoHeaderInfo -> [String] -> m ()
checkKind header allowedKinds =
  forM_ header.kind $ \kind ->
    unless (kind `elem` allowedKinds) (throwError . UserError . _ERROR_VALIDATION__LOCALE_PO_UNSUPPORTED_KIND $ kind)

toPreview :: PoHeaderInfo -> [LocaleUploadWarningDTO] -> LocaleUploadResultDTO a
toPreview header warnings =
  PreviewLocaleUploadResultDTO
    { name = header.name
    , code = header.code
    , potVersion = header.potVersion
    , warnings = warnings
    }
