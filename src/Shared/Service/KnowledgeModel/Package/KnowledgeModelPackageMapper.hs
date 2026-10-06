module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import qualified Data.UUID as U

import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple

toPackageEvent :: U.UUID -> U.UUID -> KnowledgeModelEvent -> KnowledgeModelPackageEvent
toPackageEvent packageUuid tenantUuid KnowledgeModelEvent {..} = KnowledgeModelPackageEvent {..}

toPackageRawEvent :: U.UUID -> U.UUID -> KnowledgeModelRawEvent -> KnowledgeModelPackageRawEvent
toPackageRawEvent packageUuid tenantUuid KnowledgeModelRawEvent {..} = KnowledgeModelPackageRawEvent {..}

toEvent :: KnowledgeModelPackageEvent -> KnowledgeModelEvent
toEvent KnowledgeModelPackageEvent {..} = KnowledgeModelEvent {..}

toRawEvent :: KnowledgeModelPackageRawEvent -> KnowledgeModelRawEvent
toRawEvent KnowledgeModelPackageRawEvent {..} = KnowledgeModelRawEvent {..}

toSimple :: KnowledgeModelPackage -> KnowledgeModelPackageSimple
toSimple pkg =
  KnowledgeModelPackageSimple
    { uuid = pkg.uuid
    , name = pkg.name
    , version = pkg.version
    }

toKnowledgeModelBundlePackage :: KnowledgeModelPackage -> [KnowledgeModelPackageEvent] -> Maybe Coordinate -> KnowledgeModelBundlePackage
toKnowledgeModelBundlePackage pkg pkgEvents previousPackageCoordinate =
  KnowledgeModelBundlePackage
    { name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , phase = pkg.phase
    , metamodelVersion = pkg.metamodelVersion
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , language = pkg.language
    , previousPackageId = previousPackageCoordinate
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , nonEditable = pkg.nonEditable
    , events = fmap toEvent pkgEvents
    , createdAt = pkg.createdAt
    }

fromKnowledgeModelBundlePackage :: KnowledgeModelBundlePackage -> U.UUID -> Maybe U.UUID -> U.UUID -> Maybe U.UUID -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromKnowledgeModelBundlePackage dto pkgUuid previousPackageUuid tenantUuid workspaceUuid =
  ( KnowledgeModelPackage
      { uuid = pkgUuid
      , name = dto.name
      , id = dto.id
      , version = dto.version
      , phase = dto.phase
      , metamodelVersion = dto.metamodelVersion
      , description = dto.description
      , readme = dto.readme
      , license = dto.license
      , language = dto.language
      , previousPackageUuid = previousPackageUuid
      , forkOfPackageId = dto.forkOfPackageId
      , mergeCheckpointPackageId = dto.mergeCheckpointPackageId
      , nonEditable = dto.nonEditable
      , public = False
      , tenantUuid = tenantUuid
      , workspaceUuid = workspaceUuid
      , createdAt = dto.createdAt
      }
  , fmap (toPackageEvent pkgUuid tenantUuid) dto.events
  )
