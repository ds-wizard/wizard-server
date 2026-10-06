module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Shared.Database.DAO.Library.LibraryDependentDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Library.LibraryDependents
import Shared.Model.Settings.Settings
import Shared.Service.Acl.LibraryAcl
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import qualified Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper as KnowledgeModelLocaleMapper
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAcl
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil
import Shared.Service.Library.LibraryDependentMapper
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Tenant.Limit.WizardLimitService

getPackagesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page KnowledgeModelPackageSimpleDTO)
getPackagesPage mId mQuery mOutdated pageable sort = do
  tcRegistry <- getCurrentSettingsRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "knowledgeModelPackages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mId mQuery mOutdated pageable sort
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
  tcRegistry <- getCurrentSettingsRegistry
  locales <- findKnowledgeModelLocalesByPackageUuid pkgUuid
  return $ toDetailDTO pkg tcRegistry.enabled pkgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs) (fmap KnowledgeModelLocaleMapper.toList locales)

getPackageDependents :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m LibraryDependents
getPackageDependents uuid mAllVersions = do
  checkManagePermissionToPackage uuid
  pkgUuids <- getPackageUuidsToDelete uuid mAllVersions
  toLibraryDependents <$> findPackageDependents pkgUuids

createPackage :: WizardRequestContextC s m => (KnowledgeModelPackage, [KnowledgeModelPackageEvent]) -> m KnowledgeModelPackageSimpleDTO
createPackage (pkg, pkgEvents) =
  runInTransaction $ do
    checkPackageLimit pkg.id
    insertPackage pkg
    traverse_ insertPackageEvent pkgEvents
    return . toSimpleDTO $ pkg

modifyPackage :: WizardRequestContextC s m => U.UUID -> KnowledgeModelPackageChangeDTO -> m KnowledgeModelPackageChangeDTO
modifyPackage pkgUuid reqDto =
  runInTransaction $ do
    checkManagePermissionToPackage pkgUuid
    updatePackagePhaseAndPublicByUuid pkgUuid reqDto.phase reqDto.public
    return reqDto

deletePackage :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m ()
deletePackage uuid mAllVersions =
  runInTransaction $ do
    checkManagePermissionToPackage uuid
    pkgUuids <- getPackageUuidsToDelete uuid mAllVersions
    dependents <- toLibraryDependents <$> findPackageDependents pkgUuids
    checkDeleteAllowed dependents
    traverse_ deletePackageByUuid pkgUuids
    traverse_ (logOutOnlineUsersWhenProjectDramaticallyChanged . (.uuid)) dependents.projects
    traverse_ (logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged . (.uuid)) dependents.editors

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: WizardRequestContextC s m => KnowledgeModelPackage -> Bool -> m [(U.UUID, String)]
getPackageVersions pkg excludeDeprecatedVersions = do
  allPkgs <- findPackagesByIdInWorkspace pkg.id pkg.workspaceUuid
  return . fmap (\p -> (p.uuid, p.version)) . filter (filterPkg excludeDeprecatedVersions) $ allPkgs
  where
    filterPkg :: Bool -> KnowledgeModelPackage -> Bool
    filterPkg True pkg = pkg.phase == ReleasedKnowledgeModelPackagePhase
    filterPkg False _ = True

getPackageUuidsToDelete :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m [U.UUID]
getPackageUuidsToDelete uuid mAllVersions = do
  pkg <- findPackageByUuid uuid
  case mAllVersions of
    Just True -> fmap (.uuid) <$> findPackagesByIdInWorkspace pkg.id pkg.workspaceUuid
    _ -> return [pkg.uuid]
