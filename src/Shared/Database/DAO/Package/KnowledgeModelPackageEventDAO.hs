module Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageEvent ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRawEvent ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent

entityName = "knowledge_model_package_event"

findPackageEvents :: RequestContextC s sc m => U.UUID -> m [KnowledgeModelPackageEvent]
findPackageEvents pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)] [Sort "created_at" Ascending]

findPackageRawEvents :: RequestContextC s sc m => U.UUID -> m [KnowledgeModelPackageRawEvent]
findPackageRawEvents pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)] [Sort "created_at" Ascending]

insertPackageEvent :: RequestContextC s sc m => KnowledgeModelPackageEvent -> m Int64
insertPackageEvent event = do
  createInsertFn entityName event

insertPackageRawEvent :: RequestContextC s sc m => KnowledgeModelPackageRawEvent -> m Int64
insertPackageRawEvent event = do
  createInsertFn entityName event

deletePackageEventsByPackageUuid :: RequestContextC s sc m => U.UUID -> m Int64
deletePackageEventsByPackageUuid pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)]
