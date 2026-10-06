module Shared.Api.Resource.AuditLog.AuditLogDTO where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.AuditLog.AuditLogSource

data AuditLogDTO = AuditLogDTO
  { uuid :: U.UUID
  , component :: String
  , action :: String
  , actionData :: Value
  , workspace :: Maybe AuditLogWorkspaceDTO
  , createdBy :: Maybe AuditLogUserDTO
  , source :: AuditLogSource
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data AuditLogWorkspaceDTO = AuditLogWorkspaceDTO
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Show, Eq, Generic)

data AuditLogUserDTO = AuditLogUserDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)
