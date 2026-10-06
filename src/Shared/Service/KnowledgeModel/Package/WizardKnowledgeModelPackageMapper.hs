module Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper where

import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Registry.RegistryPackage
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil
import Shared.Service.Version.VersionMapper
import Shared.Util.Reference

toSimpleDTO :: KnowledgeModelPackage -> KnowledgeModelPackageSimpleDTO
toSimpleDTO = toSimpleDTO' []

toSimpleDTO' :: [RegistryPackage] -> KnowledgeModelPackage -> KnowledgeModelPackageSimpleDTO
toSimpleDTO' pkgRs pkg =
  KnowledgeModelPackageSimpleDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , phase = pkg.phase
    , remoteLatestVersion =
        case selectPackageById pkg pkgRs of
          Just pkgR -> Just pkgR.remoteVersion
          Nothing -> Nothing
    , description = pkg.description
    , nonEditable = pkg.nonEditable
    , public = pkg.public
    , language = pkg.language
    , createdAt = pkg.createdAt
    , workspaceUuid = pkg.workspaceUuid
    }

toSimpleDTO'' :: Bool -> KnowledgeModelPackageList -> KnowledgeModelPackageSimpleDTO
toSimpleDTO'' registryEnabled pkg =
  KnowledgeModelPackageSimpleDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , phase = pkg.phase
    , remoteLatestVersion =
        if registryEnabled
          then pkg.remoteVersion
          else Nothing
    , description = pkg.description
    , nonEditable = pkg.nonEditable
    , public = pkg.public
    , language = pkg.language
    , createdAt = pkg.createdAt
    , workspaceUuid = pkg.workspaceUuid
    }

toDetailDTO :: KnowledgeModelPackage -> Bool -> [RegistryPackage] -> [(U.UUID, String)] -> Maybe String -> [KnowledgeModelLocaleList] -> KnowledgeModelPackageDetailDTO
toDetailDTO pkg registryEnabled pkgRs versionLs registryLink locales =
  KnowledgeModelPackageDetailDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , language = pkg.language
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageUuid = pkg.previousPackageUuid
    , forkOfPackageId = fmap (.id) pkg.forkOfPackageId
    , forkOfPackageVersion = fmap (.version) pkg.forkOfPackageId
    , mergeCheckpointPackageId = fmap (.id) pkg.mergeCheckpointPackageId
    , mergeCheckpointPackageVersion = fmap (.version) pkg.mergeCheckpointPackageId
    , nonEditable = pkg.nonEditable
    , public = pkg.public
    , versions = map toVersionDTO . L.sortBy (\(_, v1) (_, v2) -> compare v2 v1) $ versionLs
    , locales = locales
    , remoteLatestVersion =
        case (registryEnabled, selectPackageById pkg pkgRs) of
          (True, Just pkgR) -> Just pkgR.remoteVersion
          _ -> Nothing
    , registryLink =
        if registryEnabled
          then registryLink
          else Nothing
    , createdAt = pkg.createdAt
    , workspaceUuid = pkg.workspaceUuid
    }

toSuggestion :: KnowledgeModelPackage -> KnowledgeModelPackageSuggestion
toSuggestion pkg =
  KnowledgeModelPackageSuggestion
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , description = pkg.description
    }

toChangeDTO :: KnowledgeModelPackage -> KnowledgeModelPackageChangeDTO
toChangeDTO pkg =
  KnowledgeModelPackageChangeDTO
    { phase = pkg.phase
    , public = pkg.public
    }

buildPackageUrl :: String -> KnowledgeModelPackage -> [RegistryPackage] -> Maybe String
buildPackageUrl clientRegistryUrl pkg pkgRs =
  case selectPackageById pkg pkgRs of
    Just pkgR ->
      Just $
        clientRegistryUrl
          ++ "/knowledge-models/"
          ++ buildReference pkgR.id pkgR.remoteVersion
    Nothing -> Nothing
