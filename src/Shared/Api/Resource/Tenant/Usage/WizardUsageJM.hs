module Shared.Api.Resource.Tenant.Usage.WizardUsageJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.Usage.UsageEntryJM ()
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON WizardUsageDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WizardUsageDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema WizardUsageDTO where
  jsonSchema = genericJsonSchema jsonOptions
