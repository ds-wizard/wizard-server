module Shared.Model.Tenant.Limit.TenantLimitBundleChange where

import Data.Aeson
import GHC.Generics
import GHC.Int

import Shared.Util.Aeson

data TenantLimitBundleChange = TenantLimitBundleChange
  { users :: Int
  , activeUsers :: Int
  , knowledgeModels :: Int
  , knowledgeModelEditors :: Int
  , documentTemplates :: Int
  , documentTemplateDrafts :: Int
  , projects :: Int
  , documents :: Int
  , locales :: Int
  , storage :: Int64
  }
  deriving (Show, Eq, Generic)

instance FromJSON TenantLimitBundleChange where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantLimitBundleChange where
  toJSON = genericToJSON jsonOptions
