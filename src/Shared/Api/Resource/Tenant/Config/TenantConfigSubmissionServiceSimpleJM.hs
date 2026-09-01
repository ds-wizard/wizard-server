module Shared.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.Config.TenantConfigJM ()
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Util.Aeson

instance FromJSON TenantConfigSubmissionServiceSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceSimple where
  toJSON = genericToJSON jsonOptions
