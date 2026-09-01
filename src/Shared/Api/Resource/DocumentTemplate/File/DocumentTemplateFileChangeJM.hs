module Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateFileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileChangeDTO where
  toJSON = genericToJSON jsonOptions
