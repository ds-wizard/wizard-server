module Shared.Api.Resource.Locale.Common.LocaleUploadJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Servant.Multipart

import Shared.Api.Resource.Locale.Common.LocaleUploadDTO

instance FromMultipart Mem LocaleUploadDTO where
  fromMultipart form =
    LocaleUploadDTO
      <$> fmap (BSL.toStrict . fdPayload) (lookupFile "file" form)
      <*> pure (either (const False) (== "true") (lookupInput "confirmed" form))
