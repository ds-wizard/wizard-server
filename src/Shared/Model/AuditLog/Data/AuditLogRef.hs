module Shared.Model.AuditLog.Data.AuditLogRef where

import Data.Aeson
import Data.Swagger
import qualified Data.UUID as U
import GHC.Generics

data AuditLogUserRef = AuditLogUserRef
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogUserRef

instance ToSchema AuditLogUserRef

data AuditLogRef = AuditLogRef
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogRef

instance ToSchema AuditLogRef

data AuditLogKnowledgeModelRef = AuditLogKnowledgeModelRef
  { uuid :: U.UUID
  , id :: String
  , version :: String
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogKnowledgeModelRef

instance ToSchema AuditLogKnowledgeModelRef

data AuditLogDocumentTemplateRef = AuditLogDocumentTemplateRef
  { uuid :: U.UUID
  , id :: String
  , version :: String
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuditLogDocumentTemplateRef

instance ToSchema AuditLogDocumentTemplateRef
