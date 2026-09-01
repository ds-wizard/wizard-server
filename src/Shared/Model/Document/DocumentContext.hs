module Shared.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Model.Registry.RegistryOrganization
import Shared.Model.Report.Report
import Shared.Model.Tenant.Config.WizardTenantConfig

data DocumentContext = DocumentContext
  { config :: DocumentContextConfig
  , document :: DocumentContextDocument
  , project :: DocumentContextProject
  , knowledgeModel :: KnowledgeModel
  , report :: Report
  , knowledgeModelPackage :: DocumentContextPackage
  , organization :: TenantConfigOrganization
  , metamodelVersion :: SemVer2Tuple
  , users :: [DocumentContextUserPerm]
  , groups :: [DocumentContextUserGroupPerm]
  }
  deriving (Show, Eq, Generic)

data DocumentContextConfig = DocumentContextConfig
  { clientUrl :: String
  , appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , illustrationsColor :: Maybe String
  , primaryColor :: Maybe String
  , logoUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

data DocumentContextPackage = DocumentContextPackage
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , description :: String
  , organization :: Maybe RegistryOrganization
  , language :: String
  , createdAt :: UTCTime
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

data DocumentContextProject = DocumentContextProject
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , replies :: M.Map String Reply
  , phaseUuid :: Maybe U.UUID
  , labels :: M.Map String [U.UUID]
  , versionUuid :: Maybe U.UUID
  , versions :: [ProjectVersionList]
  , projectTags :: [String]
  , files :: [ProjectFileSimple]
  , language :: Maybe String
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocument = DocumentContextDocument
  { uuid :: U.UUID
  , name :: String
  , documentTemplateUuid :: U.UUID
  , formatUuid :: U.UUID
  , language :: Maybe String
  , locale :: Maybe DocumentContextDocumentTemplateLocale
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
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

data DocumentContextUserPerm = DocumentContextUserPerm
  { user :: DocumentContextUser
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroupPerm = DocumentContextUserGroupPerm
  { group :: UserGroupDetailDTO
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)
