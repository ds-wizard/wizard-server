module Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Util.Aeson

instance FromJSON DocumentTemplateFormatSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatSimple where
  toJSON = genericToJSON jsonOptions
