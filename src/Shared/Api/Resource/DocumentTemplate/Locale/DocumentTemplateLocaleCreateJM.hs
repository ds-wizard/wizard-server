module Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Servant.Multipart

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO

instance FromMultipart Mem DocumentTemplateLocaleCreateDTO where
  fromMultipart form =
    DocumentTemplateLocaleCreateDTO
      <$> fmap T.unpack (lookupInput "name" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "poContent" form)
