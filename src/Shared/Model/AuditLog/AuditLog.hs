module Shared.Model.AuditLog.AuditLog where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.AuditLog.AuditLogSource

data AuditLog = AuditLog
  { uuid :: U.UUID
  , component :: String
  , action :: String
  , actionData :: Value
  , workspaceUuid :: Maybe U.UUID
  , workspaceName :: Maybe String
  , createdByUuid :: Maybe U.UUID
  , createdByFirstName :: Maybe String
  , createdByLastName :: Maybe String
  , createdByEmail :: Maybe String
  , createdByImageUrl :: Maybe String
  , sourceType :: AuditLogSourceType
  , sourceUuid :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
