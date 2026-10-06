module RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimpleDTO where
  toJSON = genericToJSON jsonOptions
