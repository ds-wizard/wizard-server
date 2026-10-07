module Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO where

import qualified Data.UUID as U
import GHC.Generics

data LocaleUploadResultDTO a
  = PreviewLocaleUploadResultDTO
      { name :: String
      , code :: String
      , potVersion :: Maybe String
      , warnings :: [LocaleUploadWarningDTO]
      }
  | StoredLocaleUploadResultDTO
      { replaced :: Bool
      , locale :: a
      }
  deriving (Show, Eq, Generic)

data LocaleUploadWarningDTO = ReplaceLocaleUploadWarningDTO
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , official :: Bool
  }
  deriving (Show, Eq, Generic)
