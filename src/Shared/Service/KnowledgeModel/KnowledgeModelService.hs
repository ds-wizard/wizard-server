module Shared.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelCache
import Shared.Service.KnowledgeModel.Compiler.Compiler
import Shared.Service.KnowledgeModel.KnowledgeModelFilter
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil

createKnowledgeModelPreview :: WizardRequestContextC s m => KnowledgeModelChangeDTO -> m KnowledgeModel
createKnowledgeModelPreview reqDto = do
  checkViewPermissionToKnowledgeModelPackage reqDto.knowledgeModelPackageUuid
  compileKnowledgeModel reqDto.events reqDto.knowledgeModelPackageUuid reqDto.tagUuids

compileKnowledgeModel :: WizardRequestContextC s m => [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> m KnowledgeModel
compileKnowledgeModel events mPkgUuid tagUuids = compileKnowledgeModelWithCaching' events mPkgUuid tagUuids True

compileKnowledgeModelWithCaching' :: WizardRequestContextC s m => [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> Bool -> m KnowledgeModel
compileKnowledgeModelWithCaching' events mPkgUuid tagUuids useCache = do
  case (events, mPkgUuid) of
    ([], Just pkgUuid) -> do
      tenantUuid <- asks (.tenantUuid')
      mKmCache <-
        if useCache
          then findKnowledgeModelCacheByUuid' pkgUuid tagUuids tenantUuid
          else return Nothing
      case mKmCache of
        Just kmCache -> return kmCache.knowledgeModel
        Nothing -> do
          allEvents <- getEvents mPkgUuid
          km <- liftEither $ compile Nothing allEvents
          let filteredKm = filterKnowledgeModel tagUuids km
          if useCache
            then do
              createdAt <- liftIO getCurrentTime
              let kmCache = KnowledgeModelCache {knowledgeModelPackageUuid = pkgUuid, tagUuids = tagUuids, knowledgeModel = filteredKm, tenantUuid = tenantUuid, createdAt = createdAt}
              insertKnowledgeModelCache kmCache
              return filteredKm
            else return filteredKm
    _ -> do
      allEvents <- getEvents mPkgUuid
      km <- liftEither $ compile Nothing allEvents
      return $ filterKnowledgeModel tagUuids km
  where
    getEvents Nothing = return events
    getEvents (Just pkgUuid) = do
      (pkgs :: [KnowledgeModelBundlePackage]) <- findSeriesOfPackagesRecursiveByUuid pkgUuid
      return $ concatMap (.events) pkgs ++ events
