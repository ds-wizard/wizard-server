module Shared.Api.Resource.Mcp.TenantContext.TenantContextJM where

import Data.Aeson

import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON TenantContextDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantContextDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantContextDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON TenantContextUserDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantContextUserDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantContextUserDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON TenantContextWorkspaceDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantContextWorkspaceDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantContextWorkspaceDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance FromJSON TenantContextSettingsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantContextSettingsDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantContextSettingsDTO where
  jsonSchema = genericJsonSchema jsonOptions
