module Shared.Api.Resource.Locale.Common.LocaleUploadDTO where

import qualified Data.ByteString.Char8 as BS

data LocaleUploadDTO = LocaleUploadDTO
  { content :: BS.ByteString
  , confirmed :: Bool
  }
