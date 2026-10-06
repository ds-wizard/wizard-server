module Shared.Model.AuditLog.Data.WorkspaceAuditLogData where

import Data.Aeson
import Data.Swagger
import GHC.Generics

import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef

data AuditLogWorkspaceData = AuditLogWorkspaceData
  { description :: Maybe String
  , primaryColor :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogWorkspaceData

instance ToSchema AuditLogWorkspaceData

data AuditLogWorkspaceChanges = AuditLogWorkspaceChanges
  { name :: Maybe (Change String)
  , description :: Maybe (Change (Maybe String))
  , primaryColor :: Maybe (Change (Maybe String))
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogWorkspaceChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogWorkspaceChanges

data AuditLogWorkspaceDeleteData = AuditLogWorkspaceDeleteData
  { projects :: Int
  , documents :: Int
  , knowledgeModelEditors :: Int
  , documentTemplateEditors :: Int
  , userGroups :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogWorkspaceDeleteData

instance ToSchema AuditLogWorkspaceDeleteData

data AuditLogWorkspaceMemberData = AuditLogWorkspaceMemberData
  { member :: AuditLogUserRef
  , role :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogWorkspaceMemberData

instance ToSchema AuditLogWorkspaceMemberData

data AuditLogWorkspaceMemberRoleChangeData = AuditLogWorkspaceMemberRoleChangeData
  { member :: AuditLogUserRef
  , oldRole :: AuditLogRef
  , newRole :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogWorkspaceMemberRoleChangeData

instance ToSchema AuditLogWorkspaceMemberRoleChangeData
