module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSuggestionDTO where
  toJSON = genericToJSON jsonOptions
