module Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService (
  getTemporaryFileWithBundle,
  exportBundle,
  pullBundleFromRegistry,
  importAndConvertBundle,
  importBundle,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.Component
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage ()
import Shared.Integration.Http.Registry.Runner
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.Localization.LocaleRecord
import Shared.Service.Acl.LibraryAcl
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAudit
import Shared.Service.KnowledgeModel.KnowledgeModelValidation
import Shared.Service.KnowledgeModel.Metamodel.MigrationService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAcl
import qualified Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as KnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation (
  validateMaybePreviousPackageIdExistence,
  validatePackageIdUniqueness,
 )
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.List
import Shared.Util.Logger
import Shared.Util.Uuid

getTemporaryFileWithBundle :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getTemporaryFileWithBundle uuid =
  runInTransaction $ do
    bundle <- exportBundle uuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.km" [show . createCoordinate $ bundle]) "application/octet-stream" mCurrentUserUuid (encode bundle)
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelBundle
exportBundle uuid =
  runInTransaction $ do
    checkManagePermissionToPackage uuid
    packages <- findSeriesOfPackagesRecursiveByUuid uuid
    case lastSafe packages of
      Just newestPackage -> do
        when
          newestPackage.nonEditable
          (throwError . UserError $ _ERROR_SERVICE_PKG__NON_EDITABLE_PKG)
        let bundle =
              KnowledgeModelBundle
                { name = newestPackage.name
                , id = newestPackage.id
                , version = newestPackage.version
                , metamodelVersion = knowledgeModelMetamodelVersion
                , packages = packages
                }
        auditKnowledgeModelBundleExport (show (createCoordinate bundle))
        return bundle
      Nothing -> throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG (show uuid)

pullBundleFromRegistry :: WizardRequestContextC s m => String -> m KnowledgeModelPackageSimpleDTO
pullBundleFromRegistry pkgId =
  runInTransaction $ do
    workspaceUuid <- requireTenantOrWorkspaceScope
    checkLibraryPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION workspaceUuid
    pb <- catchError (retrieveKnowledgeModelBundleById pkgId) handleError
    importAndConvertBundle pb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId
        else throwError error

importAndConvertBundle :: WizardRequestContextC s m => BSL.ByteString -> Bool -> m KnowledgeModelPackageSimpleDTO
importAndConvertBundle contentS fromRegistry =
  runInTransaction $ do
    workspaceUuid <- requireTenantOrWorkspaceScope
    checkLibraryPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION workspaceUuid
    case eitherDecode contentS of
      Right content -> do
        encodedPb <- migrateKnowledgeModelBundle content
        case eitherDecode . encode $ encodedPb of
          Right pb -> do
            if fromRegistry
              then auditKnowledgeModelBundlePullFromRegistry (show (createCoordinate pb))
              else auditKnowledgeModelBundleImportFromFile (show (createCoordinate pb))
            importBundle pb workspaceUuid
          Left error -> do
            logWarnI _CMP_SERVICE ("Could not deserialize migrated Knowledge Model Bundle content (" ++ show error ++ ")")
            throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
      Left error -> do
        logWarnI _CMP_SERVICE ("Could not deserialize Knowledge Model Bundle content (" ++ show error ++ ")")
        throwError . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

importBundle :: WizardRequestContextC s m => KnowledgeModelBundle -> Maybe U.UUID -> m KnowledgeModelPackageSimpleDTO
importBundle pb workspaceUuid =
  runInTransaction $ do
    pkg <- extractMainPackage pb
    forM_ pb.packages (validateIdentifierFormat "id" . (.id))
    validatePackageIdUniqueness (createCoordinate pkg) workspaceUuid
    pkgs <- forM pb.packages (importPackage workspaceUuid)
    case lastSafe . catMaybes $ pkgs of
      Just createdPkg -> return createdPkg
      Nothing -> throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show . createCoordinate $ pkg)
  where
    extractMainPackage pb =
      case find (\p -> createCoordinate p == createCoordinate pb) pb.packages of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: WizardRequestContextC s m => Maybe U.UUID -> KnowledgeModelBundlePackage -> m (Maybe KnowledgeModelPackageSimpleDTO)
importPackage workspaceUuid dto =
  runInTransaction $ do
    tenantUuid <- asks (.tenantUuid')
    uuid <- liftIO generateUuid
    validateMaybePreviousPackageIdExistence (createCoordinate dto) dto.previousPackageId workspaceUuid
    previousPackage <- traverse (`findPackageByCoordinate` workspaceUuid) dto.previousPackageId
    let (pkg, kmEvents) = KnowledgeModelPackageMapper.fromKnowledgeModelBundlePackage dto uuid (fmap (.uuid) previousPackage) tenantUuid workspaceUuid
    skipIfPackageIsAlreadyImported pkg $ do
      let events = fmap KnowledgeModelPackageMapper.toEvent kmEvents
      validateKmValidity events pkg.previousPackageUuid
      let fixedKmEvents = fixTimestampsIfNeeded kmEvents
      createdPkg <- catchError (createPackage (pkg, fixedKmEvents)) (handleCreatePackageError pkg)
      return . Just $ createdPkg
  where
    skipIfPackageIsAlreadyImported pkg callback = do
      eitherPackage <- findPackageByCoordinate' (createCoordinate pkg) pkg.workspaceUuid
      case eitherPackage of
        Nothing -> callback
        Just _ -> return Nothing
    handleCreatePackageError pkg (UserError (LocaleRecord "error.database.unique_constraint_violation" _ _)) =
      throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show . createCoordinate $ pkg)
    handleCreatePackageError _ error = throwError error

fixTimestampsIfNeeded :: [KnowledgeModelPackageEvent] -> [KnowledgeModelPackageEvent]
fixTimestampsIfNeeded [] = []
fixTimestampsIfNeeded events@(first : _) =
  if isSortedByCreatedAt events
    then events
    else zipWith setNewTime events [0 ..]
  where
    baseTime = first.createdAt
    setNewTime e i = e {createdAt = addUTCTime (fromIntegral i) baseTime} :: KnowledgeModelPackageEvent

isSortedByCreatedAt :: [KnowledgeModelPackageEvent] -> Bool
isSortedByCreatedAt [] = True
isSortedByCreatedAt [_] = True
isSortedByCreatedAt (x : y : rest) = x.createdAt < y.createdAt && isSortedByCreatedAt (y : rest)
