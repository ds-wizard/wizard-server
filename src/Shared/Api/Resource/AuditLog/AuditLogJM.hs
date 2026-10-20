module Shared.Api.Resource.AuditLog.AuditLogJM where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T

import Shared.Api.Resource.AuditLog.AuditLogDTO
import Shared.Model.AuditLog.AuditLogSource
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON AuditLogSourceType where
  toJSON = String . T.pack . auditLogSourceTypeName

instance FromJSON AuditLogSourceType where
  parseJSON (String name) = maybe mzero return . parseAuditLogSourceType . T.unpack $ name
  parseJSON _ = mzero

instance ToJSON AuditLogSource where
  toJSON = genericToJSON jsonOptions

instance FromJSON AuditLogSource where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AuditLogDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON AuditLogDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AuditLogWorkspaceDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON AuditLogWorkspaceDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AuditLogUserDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON AuditLogUserDTO where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema AuditLogSourceType where
  jsonSchema _ = enumSchema (fmap auditLogSourceTypeName [minBound .. maxBound])

instance JsonSchema AuditLogSource where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema AuditLogDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema AuditLogWorkspaceDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema AuditLogUserDTO where
  jsonSchema = genericJsonSchema jsonOptions
