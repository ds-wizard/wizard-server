module Shared.Model.AuditLog.Data.UserAuditLogData where

import Data.Aeson
import Data.Swagger
import GHC.Generics

import Shared.Model.AuditLog.Data.AuditLogRef

data AuditLogUserData = AuditLogUserData
  { firstName :: String
  , lastName :: String
  , affiliation :: Maybe String
  , role :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserData

instance ToSchema AuditLogUserData
