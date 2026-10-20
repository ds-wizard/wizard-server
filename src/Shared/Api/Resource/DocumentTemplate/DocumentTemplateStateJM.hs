module Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Util.JsonSchema

instance FromJSON DocumentTemplateState

instance ToJSON DocumentTemplateState

instance JsonSchema DocumentTemplateState where
  jsonSchema = genericJsonSchema defaultOptions
