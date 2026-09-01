module Shared.Service.KnowledgeModel.Package.Event.KnowledgeModelPackageEventService where

import qualified Data.UUID as U

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

getAllPreviousEventsSincePackageId :: WizardRequestContextC s m => U.UUID -> m [KnowledgeModelEvent]
getAllPreviousEventsSincePackageId pkgUuid = do
  package <- findPackageByUuid pkgUuid
  packageEvents <- findPackageEvents pkgUuid
  case package.previousPackageUuid of
    Just previousPackageUuid -> do
      pkgEvents <- getAllPreviousEventsSincePackageId previousPackageUuid
      return $ pkgEvents ++ fmap toEvent packageEvents
    Nothing -> return (fmap toEvent packageEvents)

getAllPreviousEventsSincePackageIdAndUntilPackageId :: WizardRequestContextC s m => U.UUID -> U.UUID -> m [KnowledgeModelEvent]
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgUuid untilPkgUuid = go sincePkgUuid
  where
    go pkgUuid =
      if pkgUuid == untilPkgUuid
        then return []
        else do
          package <- findPackageByUuid pkgUuid
          packageEvents <- findPackageEvents pkgUuid
          case package.previousPackageUuid of
            Just previousPackageUuid -> do
              pkgEvents <- go previousPackageUuid
              return $ pkgEvents ++ fmap toEvent packageEvents
            Nothing -> return (fmap toEvent packageEvents)
