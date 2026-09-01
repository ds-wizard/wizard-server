module Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Version.VersionDTO
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Model.Registry.RegistryOrganization

data DocumentTemplateDetailDTO = DocumentTemplateDetailDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormat]
  , nonEditable :: Bool
  , language :: String
  , potFileReady :: Bool
  , usableKnowledgeModels :: [KnowledgeModelPackageSimpleDTO]
  , locales :: [DocumentTemplateLocaleList]
  , versions :: [VersionDTO]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , state :: DocumentTemplateState
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
