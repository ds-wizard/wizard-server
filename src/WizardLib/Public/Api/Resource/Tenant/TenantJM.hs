module WizardLib.Public.Api.Resource.Tenant.TenantJM where

import Data.Aeson
import Servant.API

import Shared.Common.Api.Resource.Common.FromHttpApiData
import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.Tenant

instance FromJSON Tenant where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Tenant where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantState where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantState where
  toJSON = genericToJSON jsonOptions

instance FromHttpApiData [TenantState] where
  parseQueryParam = genericParseQueryParams
