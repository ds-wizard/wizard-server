module Shared.Model.DocumentTemplate.DocumentTemplate where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Model.Common.SemVer2Tuple
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplatePhase
  = DraftDocumentTemplatePhase
  | ReleasedDocumentTemplatePhase
  | DeprecatedDocumentTemplatePhase
  deriving (Show, Eq, Generic, Read)

data DocumentTemplate = DocumentTemplate
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , nonEditable :: Bool
  , language :: String
  , potFileReady :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory DocumentTemplate where
  createCoordinate dt =
    Coordinate
      { id = dt.id
      , version = dt.version
      }

data DocumentTemplateFormat = DocumentTemplateFormat
  { documentTemplateUuid :: U.UUID
  , uuid :: U.UUID
  , name :: String
  , icon :: String
  , steps :: [DocumentTemplateFormatStep]
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateFormat where
  compare f1 f2 = compare f1.name f2.name

data DocumentTemplateFormatStep = DocumentTemplateFormatStep
  { documentTemplateUuid :: U.UUID
  , formatUuid :: U.UUID
  , position :: Int
  , name :: String
  , options :: M.Map String String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFile = DocumentTemplateFile
  { documentTemplateUuid :: U.UUID
  , uuid :: U.UUID
  , fileName :: String
  , content :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateAsset = DocumentTemplateAsset
  { documentTemplateUuid :: U.UUID
  , uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplate where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version
