module Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateBundleDTO = DocumentTemplateBundleDTO
  { id :: String
  , name :: String
  , version :: String
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , language :: String
  , formats :: [DocumentTemplateFormatDTO]
  , files :: [DocumentTemplateFileDTO]
  , assets :: [DocumentTemplateAssetDTO]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory DocumentTemplateBundleDTO where
  createCoordinate dt =
    Coordinate
      { id = dt.id
      , version = dt.version
      }
