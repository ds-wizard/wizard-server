module Shared.Api.Resource.Tenant.TenantSuggestionJM where

import Data.Aeson

import Shared.Model.Tenant.TenantSuggestion
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON TenantSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantSuggestion where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantSuggestion where
  jsonSchema = genericJsonSchema jsonOptions
