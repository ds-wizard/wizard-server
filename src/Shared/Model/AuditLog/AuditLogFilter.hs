module Shared.Model.AuditLog.AuditLogFilter where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.AuditLog.AuditLogSource

data AuditLogWorkspaceFilter
  = AllAuditLogWorkspaces
  | OrganizationAuditLogWorkspaces
  | SingleAuditLogWorkspace U.UUID
  deriving (Show, Eq)

data AuditLogFilter = AuditLogFilter
  { query :: Maybe String
  , components :: Maybe [String]
  , actions :: Maybe [String]
  , createdBy :: Maybe U.UUID
  , source :: Maybe AuditLogSourceType
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  , workspace :: AuditLogWorkspaceFilter
  }
  deriving (Show, Eq)
