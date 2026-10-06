module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import qualified Data.List as L

import qualified Data.UUID as U

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Util.List (groupBy)
import Shared.Util.Reference

groupPackages :: [KnowledgeModelPackage] -> [[KnowledgeModelPackage]]
groupPackages = groupBy (\p1 p2 -> p1.id == p2.id)

sortPackagesByVersion :: [KnowledgeModelPackage] -> [KnowledgeModelPackage]
sortPackagesByVersion = L.sortBy (\p1 p2 -> compareVersionNeg p1.version p2.version)

fitsIntoKMSpecs :: Coordinate -> [KnowledgeModelPackagePattern] -> Bool
fitsIntoKMSpecs coordinate = foldl (go coordinate) False
  where
    go :: Coordinate -> Bool -> KnowledgeModelPackagePattern -> Bool
    go coordinate acc packagePattern = acc || fitsIntoKMSpec coordinate packagePattern

fitsIntoKMSpec :: Coordinate -> KnowledgeModelPackagePattern -> Bool
fitsIntoKMSpec coordinate kmSpec = heCompareId $ heCompareVersionMin $ heCompareVersionMax True
  where
    heCompareId callback =
      case kmSpec.id of
        Just pkgId -> (coordinate.id == pkgId) && callback
        Nothing -> callback
    heCompareVersionMin callback =
      case kmSpec.minVersion of
        Just minVersion ->
          case compareVersion coordinate.version minVersion of
            LT -> False
            _ -> callback
        Nothing -> callback
    heCompareVersionMax callback =
      case kmSpec.maxVersion of
        Just maxVersion ->
          case compareVersion coordinate.version maxVersion of
            GT -> False
            _ -> callback
        Nothing -> callback

resolvePackageCoordinate :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m KnowledgeModelPackage
resolvePackageCoordinate coordinate mWorkspaceUuid =
  if coordinate.version == "latest"
    then findLatestPackageById coordinate.id Nothing mWorkspaceUuid
    else findPackageByCoordinate coordinate mWorkspaceUuid
