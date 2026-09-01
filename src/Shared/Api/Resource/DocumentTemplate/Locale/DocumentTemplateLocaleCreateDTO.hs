module Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO where

import qualified Data.ByteString.Char8 as BS
import GHC.Generics

data DocumentTemplateLocaleCreateDTO = DocumentTemplateLocaleCreateDTO
  { name :: String
  , poContent :: BS.ByteString
  }
  deriving (Show, Eq, Generic)
