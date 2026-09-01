module Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateChangeDTO where
  toJSON = genericToJSON jsonOptions
