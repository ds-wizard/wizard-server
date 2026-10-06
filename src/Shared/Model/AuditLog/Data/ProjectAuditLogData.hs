module Shared.Model.AuditLog.Data.ProjectAuditLogData where

import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Api.Resource.Submission.SubmissionSM ()
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Submission.Submission

data AuditLogProjectCreateData = AuditLogProjectCreateData
  { knowledgeModel :: AuditLogKnowledgeModelRef
  , projectTemplate :: Maybe AuditLogRef
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectCreateData

instance ToSchema AuditLogProjectCreateData

newtype AuditLogProjectCloneData = AuditLogProjectCloneData
  { sourceProject :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectCloneData

instance ToSchema AuditLogProjectCloneData

data AuditLogProjectImportData = AuditLogProjectImportData
  { knowledgeModel :: AuditLogKnowledgeModelRef
  , owners :: [AuditLogUserRef]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectImportData

instance ToSchema AuditLogProjectImportData

data AuditLogProjectChanges = AuditLogProjectChanges
  { name :: Maybe (Change String)
  , description :: Maybe (Change (Maybe String))
  , visibility :: Maybe (Change ProjectVisibility)
  , sharing :: Maybe (Change ProjectSharing)
  , projectTags :: Maybe (Change [String])
  , isTemplate :: Maybe (Change Bool)
  , documentTemplate :: Maybe (Change (Maybe AuditLogDocumentTemplateRef))
  , format :: Maybe (Change (Maybe AuditLogRef))
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogProjectChanges

data AuditLogProjectPermissionsData = AuditLogProjectPermissionsData
  { added :: [AuditLogProjectMember]
  , removed :: [AuditLogProjectMember]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectPermissionsData

instance ToSchema AuditLogProjectPermissionsData

data AuditLogProjectMember = AuditLogProjectMember
  { memberType :: ProjectPermType
  , member :: AuditLogRef
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectMember

instance ToSchema AuditLogProjectMember

newtype AuditLogProjectVersionData = AuditLogProjectVersionData
  { version :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectVersionData

instance ToSchema AuditLogProjectVersionData

data AuditLogProjectVersionChanges = AuditLogProjectVersionChanges
  { name :: Maybe (Change String)
  , description :: Maybe (Change (Maybe String))
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectVersionChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogProjectVersionChanges

data AuditLogProjectRevertData = AuditLogProjectRevertData
  { eventUuid :: U.UUID
  , eventCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectRevertData

instance ToSchema AuditLogProjectRevertData

data AuditLogProjectMigrateData = AuditLogProjectMigrateData
  { sourceKnowledgeModel :: AuditLogKnowledgeModelRef
  , targetKnowledgeModel :: AuditLogKnowledgeModelRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectMigrateData

instance ToSchema AuditLogProjectMigrateData

data AuditLogDocumentCreateData = AuditLogDocumentCreateData
  { project :: AuditLogRef
  , documentTemplate :: AuditLogDocumentTemplateRef
  , format :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogDocumentCreateData

instance ToSchema AuditLogDocumentCreateData

newtype AuditLogDocumentData = AuditLogDocumentData
  { project :: Maybe AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogDocumentData

instance ToSchema AuditLogDocumentData

data AuditLogDocumentSubmitData = AuditLogDocumentSubmitData
  { project :: Maybe AuditLogRef
  , service :: AuditLogSubmissionServiceRef
  , state :: SubmissionState
  , location :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogDocumentSubmitData

instance ToSchema AuditLogDocumentSubmitData

data AuditLogSubmissionServiceRef = AuditLogSubmissionServiceRef
  { id :: String
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogSubmissionServiceRef

instance ToSchema AuditLogSubmissionServiceRef

newtype AuditLogProjectFileData = AuditLogProjectFileData
  { project :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogProjectFileData

instance ToSchema AuditLogProjectFileData
