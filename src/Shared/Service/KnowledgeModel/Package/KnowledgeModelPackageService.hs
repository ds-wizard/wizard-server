module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.Registry.RegistryOrganizationDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Tenant.Config.WizardTenantConfig
import qualified Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper as KnowledgeModelLocaleMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil
import Shared.Service.Tenant.Config.ConfigService
import Shared.Service.Tenant.Limit.WizardLimitService

getPackagesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page KnowledgeModelPackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort = do
  tcRegistry <- getCurrentTenantConfigRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "knowledgeModelPackages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort
      return . fmap (toSimpleDTO'' tcRegistry.enabled) $ packages

getPackageSuggestions :: WizardRequestContextC s m => Maybe String -> Maybe [Coordinate] -> Maybe [Coordinate] -> Maybe KnowledgeModelPackagePhase -> Maybe Bool -> Pageable -> [Sort] -> m (Page KnowledgeModelPackageSuggestion)
getPackageSuggestions mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort = do
  findPackageSuggestionsPage mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort

getPackageDetailByUuid :: WizardRequestContextC s m => U.UUID -> Bool -> m KnowledgeModelPackageDetailDTO
getPackageDetailByUuid pkgUuid excludeDeprecatedVersions = do
  checkViewPermissionToKnowledgeModelPackage (Just pkgUuid)
  pkg <- findPackageByUuid pkgUuid
  serverConfig <- asks (.serverConfig')
  versions <- getPackageVersions pkg excludeDeprecatedVersions
  pkgRs <- findRegistryPackages
  orgRs <- findRegistryOrganizations
  tcRegistry <- getCurrentTenantConfigRegistry
  locales <- findKnowledgeModelLocalesByPackageUuid pkgUuid
  return $ toDetailDTO pkg tcRegistry.enabled pkgRs orgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs) (fmap KnowledgeModelLocaleMapper.toList locales)

getDependentPackageResources :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m [KnowledgeModelPackageDeletionImpact]
getDependentPackageResources uuid mAllVersions = do
  case mAllVersions of
    Just True -> do
      pkg <- findPackageByUuid uuid
      allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
      findDependentPackageResources (fmap (.uuid) allPkgs)
    _ -> findDependentPackageResources [uuid]

createPackage :: WizardRequestContextC s m => (KnowledgeModelPackage, [KnowledgeModelPackageEvent]) -> m KnowledgeModelPackageSimpleDTO
createPackage (pkg, pkgEvents) =
  runInTransaction $ do
    checkPackageLimit pkg.organizationId pkg.kmId
    insertPackage pkg
    traverse_ insertPackageEvent pkgEvents
    return . toSimpleDTO $ pkg

modifyPackage :: WizardRequestContextC s m => U.UUID -> KnowledgeModelPackageChangeDTO -> m KnowledgeModelPackageChangeDTO
modifyPackage pkgUuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    _ <- findPackageByUuid pkgUuid
    updatePackagePhaseAndPublicByUuid pkgUuid reqDto.phase reqDto.public
    return reqDto

deletePackage :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m ()
deletePackage uuid mAllVersions =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    case mAllVersions of
      Just True -> do
        pkg <- findPackageByUuid uuid
        void $ deletePackagesFiltered [("organization_id", pkg.organizationId), ("km_id", pkg.kmId)]
      _ -> do
        _ <- findPackageByUuid uuid
        void $ deletePackageByUuid uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: WizardRequestContextC s m => KnowledgeModelPackage -> Bool -> m [(U.UUID, String)]
getPackageVersions pkg excludeDeprecatedVersions = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . fmap (\p -> (p.uuid, p.version)) . filter (filterPkg excludeDeprecatedVersions) $ allPkgs
  where
    filterPkg :: Bool -> KnowledgeModelPackage -> Bool
    filterPkg True pkg = pkg.phase == ReleasedKnowledgeModelPackagePhase
    filterPkg False _ = True
