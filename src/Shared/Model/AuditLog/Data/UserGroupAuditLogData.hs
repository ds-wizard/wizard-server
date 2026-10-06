module Shared.Model.AuditLog.Data.UserGroupAuditLogData where

import Data.Aeson
import Data.Swagger
import GHC.Generics

import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef

data AuditLogUserGroupData = AuditLogUserGroupData
  { description :: Maybe String
  , private :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserGroupData

instance ToSchema AuditLogUserGroupData

data AuditLogUserGroupChanges = AuditLogUserGroupChanges
  { name :: Maybe (Change String)
  , description :: Maybe (Change (Maybe String))
  , private :: Maybe (Change Bool)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserGroupChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogUserGroupChanges

data AuditLogUserGroupMember = AuditLogUserGroupMember
  { user :: AuditLogUserRef
  , membershipType :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserGroupMember

instance ToSchema AuditLogUserGroupMember

data AuditLogUserGroupMemberEdit = AuditLogUserGroupMemberEdit
  { user :: AuditLogUserRef
  , membershipType :: Change String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserGroupMemberEdit

instance ToSchema AuditLogUserGroupMemberEdit

data AuditLogUserGroupMembersData = AuditLogUserGroupMembersData
  { add :: [AuditLogUserGroupMember]
  , edit :: [AuditLogUserGroupMemberEdit]
  , delete :: [AuditLogUserGroupMember]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserGroupMembersData

instance ToSchema AuditLogUserGroupMembersData
