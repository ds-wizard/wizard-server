module Shared.Model.AuditLog.Data.LibraryAuditLogData where

import Data.Aeson
import Data.Swagger
import GHC.Generics

import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef

data AuditLogLibraryData = AuditLogLibraryData
  { id :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLibraryData

instance ToSchema AuditLogLibraryData

data AuditLogLibraryPublishData = AuditLogLibraryPublishData
  { id :: String
  , version :: String
  , editor :: AuditLogRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLibraryPublishData

instance ToSchema AuditLogLibraryPublishData

data AuditLogLibraryDeleteData = AuditLogLibraryDeleteData
  { allVersions :: Bool
  , deletedPackages :: [String]
  , dependents :: AuditLogLibraryDependentsData
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLibraryDeleteData

instance ToSchema AuditLogLibraryDeleteData

data AuditLogLibraryDependentsData = AuditLogLibraryDependentsData
  { editors :: Int
  , projects :: Int
  , documents :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogLibraryDependentsData

instance ToSchema AuditLogLibraryDependentsData

data AuditLogKnowledgeModelVisibilityChanges = AuditLogKnowledgeModelVisibilityChanges
  { public :: Maybe (Change Bool)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelVisibilityChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogKnowledgeModelVisibilityChanges

data AuditLogKnowledgeModelEditorData = AuditLogKnowledgeModelEditorData
  { id :: String
  , version :: String
  , basedOn :: Maybe AuditLogKnowledgeModelRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelEditorData

instance ToSchema AuditLogKnowledgeModelEditorData

data AuditLogDocumentTemplateEditorData = AuditLogDocumentTemplateEditorData
  { id :: String
  , version :: String
  , basedOn :: Maybe AuditLogDocumentTemplateRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogDocumentTemplateEditorData

instance ToSchema AuditLogDocumentTemplateEditorData

data AuditLogEditorChanges = AuditLogEditorChanges
  { name :: Maybe (Change String)
  , id :: Maybe (Change String)
  , version :: Maybe (Change String)
  , description :: Maybe (Change String)
  , license :: Maybe (Change String)
  , language :: Maybe (Change String)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogEditorChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogEditorChanges

data AuditLogKnowledgeModelMigrationData = AuditLogKnowledgeModelMigrationData
  { sourcePackage :: AuditLogKnowledgeModelRef
  , targetPackage :: AuditLogKnowledgeModelRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelMigrationData

instance ToSchema AuditLogKnowledgeModelMigrationData

data AuditLogKnowledgeModelSecretChanges = AuditLogKnowledgeModelSecretChanges
  { name :: Maybe (Change String)
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelSecretChanges where
  toJSON = toChangesJSON

instance ToSchema AuditLogKnowledgeModelSecretChanges

data AuditLogKnowledgeModelSecretUpdateData = AuditLogKnowledgeModelSecretUpdateData
  { valueChanged :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelSecretUpdateData

instance ToSchema AuditLogKnowledgeModelSecretUpdateData
