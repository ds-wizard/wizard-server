module Shared.Model.AuditLog.Data.UserAuditLogData where

import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Data.UUID as U
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

data AuditLogSessionData = AuditLogSessionData
  { tokenUuid :: U.UUID
  , name :: String
  , userAgent :: String
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogSessionData

instance ToSchema AuditLogSessionData

data AuditLogMcpConnectData = AuditLogMcpConnectData
  { tokenUuid :: U.UUID
  , clientName :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogMcpConnectData

instance ToSchema AuditLogMcpConnectData
