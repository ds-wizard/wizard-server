module Shared.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.SemVer2Tuple
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.Report.Report
import Shared.Model.User.UserGroupMembership

data DocumentContext = DocumentContext
  { metamodelVersion :: SemVer2Tuple
  , hash :: String
  , workspace :: DocumentContextWorkspace
  , project :: Maybe DocumentContextProject
  , knowledgeModelEditor :: Maybe DocumentContextKnowledgeModelEditor
  , questionnaire :: DocumentContextQuestionnaire
  , knowledgeModel :: KnowledgeModel
  , knowledgeModelPackage :: Maybe DocumentContextPackage
  , document :: Maybe DocumentContextDocument
  , config :: DocumentContextConfig
  }
  deriving (Show, Eq, Generic)

data DocumentContextWorkspace = DocumentContextWorkspace
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  }
  deriving (Show, Eq, Generic)

data DocumentContextProject = DocumentContextProject
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , isTemplate :: Bool
  , knowledgeModelPackageUuid :: U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , projectTags :: [String]
  , documentTemplateUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , users :: [DocumentContextUserPerm]
  , groups :: [DocumentContextUserGroupPerm]
  }
  deriving (Show, Eq, Generic)

data DocumentContextKnowledgeModelEditor = DocumentContextKnowledgeModelEditor
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , dckeId :: String
  , version :: String
  , license :: String
  , readme :: String
  , previousPackageUuid :: Maybe U.UUID
  , previousPackageId :: Maybe String
  , previousPackageVersion :: Maybe String
  , dckeMetamodelVersion :: Int
  }
  deriving (Show, Eq, Generic)

data DocumentContextUser = DocumentContextUser
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , active :: Bool
  , imageUrl :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserPerm = DocumentContextUserPerm
  { user :: DocumentContextUser
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroupPerm = DocumentContextUserGroupPerm
  { group :: DocumentContextUserGroup
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroup = DocumentContextUserGroup
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  , members :: [DocumentContextUserGroupMember]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroupMember = DocumentContextUserGroupMember
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , gravatarHash :: String
  , imageUrl :: Maybe String
  , affiliation :: Maybe String
  , membershipType :: UserGroupMembershipType
  }
  deriving (Show, Eq, Generic)

data DocumentContextQuestionnaire = DocumentContextQuestionnaire
  { replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  , phaseUuid :: Maybe U.UUID
  , versionUuid :: Maybe U.UUID
  , versions :: [ProjectVersionList]
  , files :: [ProjectFileSimple]
  , report :: DocumentContextReport
  , language :: Maybe String
  }
  deriving (Show, Eq, Generic)

data DocumentContextReport = DocumentContextReport
  { totalReport :: TotalReport
  , chapterReports :: [ChapterReport]
  , chapters :: [Chapter]
  , metrics :: [Metric]
  }
  deriving (Show, Eq, Generic)

data DocumentContextPackage = DocumentContextPackage
  { uuid :: U.UUID
  , dcpId :: String
  , version :: String
  , name :: String
  , description :: String
  , language :: String
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocument = DocumentContextDocument
  { uuid :: U.UUID
  , name :: String
  , documentTemplate :: DocumentContextDocumentTemplate
  , format :: DocumentContextFormat
  , language :: Maybe String
  , locale :: Maybe DocumentContextDocumentTemplateLocale
  , preview :: Bool
  , eventUuid :: Maybe U.UUID
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocumentTemplate = DocumentContextDocumentTemplate
  { uuid :: U.UUID
  , dcdtId :: String
  , version :: String
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

data DocumentContextFormat = DocumentContextFormat
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocumentTemplateLocale = DocumentContextDocumentTemplateLocale
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextConfig = DocumentContextConfig
  { clientUrl :: String
  , appTitle :: Maybe String
  , primaryColor :: Maybe String
  , logoUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)
