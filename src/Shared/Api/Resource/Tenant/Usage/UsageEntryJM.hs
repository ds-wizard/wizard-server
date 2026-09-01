module Shared.Api.Resource.Tenant.Usage.UsageEntryJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.Usage.UsageEntryDTO
import Shared.Util.Aeson

instance FromJSON UsageEntryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UsageEntryDTO where
  toJSON = genericToJSON jsonOptions
