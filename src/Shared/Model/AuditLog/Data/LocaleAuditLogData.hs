module Shared.Model.AuditLog.Data.LocaleAuditLogData where

import Data.Aeson
import Data.Swagger
import GHC.Generics

import Shared.Model.AuditLog.AuditLogEvent

data AuditLogLocaleData = AuditLogLocaleData
  { code :: String
  , name :: String
  , potVersion :: Maybe String
  , official :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLocaleData

instance ToSchema AuditLogLocaleData

data AuditLogLocaleReplaceData = AuditLogLocaleReplaceData
  { code :: String
  , name :: String
  , oldPotVersion :: Maybe String
  , newPotVersion :: Maybe String
  , officialSwitchedOff :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLocaleReplaceData

instance ToSchema AuditLogLocaleReplaceData

data AuditLogLocaleChanges = AuditLogLocaleChanges
  { enabled :: Maybe (Change Bool)
  , defaultLocale :: Maybe (Change Bool)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLocaleChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogLocaleChanges
