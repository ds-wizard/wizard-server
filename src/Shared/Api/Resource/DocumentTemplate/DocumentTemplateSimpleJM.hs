module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Util.Aeson

instance FromJSON DocumentTemplateSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimple where
  toJSON = genericToJSON jsonOptions
