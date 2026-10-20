module Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewDTO
import Shared.Constant.KnowledgeModel
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.KnowledgeModel.Tree.KnowledgeModelEntityNodeQuery
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper (toEventChangeDTO)
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventValidation (applyEvent)
import Shared.Service.KnowledgeModel.Tree.KnowledgeModelTreeMapper
import Shared.Util.Reflection (constructorName)
import Shared.Util.String (stripSuffixIfExists)

toDTO :: KnowledgeModelMigration -> KnowledgeModelPackageSuggestion -> KnowledgeModelPackageSuggestion -> KnowledgeModelEditor -> KnowledgeModelMigrationDTO
toDTO ms previousPackage targetPackage kmEditor =
  KnowledgeModelMigrationDTO
    { editorUuid = ms.editorUuid
    , editorName = kmEditor.name
    , editorPreviousPackage = previousPackage
    , state = ms.state
    , targetPackage = targetPackage
    , currentKnowledgeModel = ms.currentKnowledgeModel
    }

fromCreateDTO :: KnowledgeModelEditor -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> U.UUID -> [KnowledgeModelEvent] -> KnowledgeModel -> U.UUID -> UTCTime -> KnowledgeModelMigration
fromCreateDTO kmEditor previousPkg editorPreviousPackageEvents targetPkgUuid targetPkgEvents km tenantUuid now =
  KnowledgeModelMigration
    { editorUuid = kmEditor.uuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , state = RunningKnowledgeModelMigrationState
    , editorPreviousPackageUuid = previousPkg.uuid
    , targetPackageUuid = targetPkgUuid
    , editorPreviousPackageEvents = editorPreviousPackageEvents
    , targetPackageEvents = targetPkgEvents
    , resultEvents = []
    , currentKnowledgeModel = Just km
    , tenantUuid = tenantUuid
    , createdAt = now
    }

toViewDTO :: KnowledgeModelMigration -> KnowledgeModelPackageSuggestion -> KnowledgeModelPackageSuggestion -> Int -> KnowledgeModelMigrationViewDTO
toViewDTO ms previousPackage targetPackage totalEvents =
  KnowledgeModelMigrationViewDTO
    { editorUuid = ms.editorUuid
    , state = stripSuffixIfExists "KnowledgeModelMigrationState" (constructorName ms.state)
    , editorPreviousPackage = previousPackage
    , targetPackage = targetPackage
    , processedEvents = totalEvents - length ms.targetPackageEvents
    , totalEvents = totalEvents
    , conflict = toConflictDTO <$> ms.currentKnowledgeModel <*> conflictEvent ms.state
    }

conflictEvent :: KnowledgeModelMigrationState -> Maybe KnowledgeModelEvent
conflictEvent (ConflictKnowledgeModelMigrationState mEvent) = mEvent
conflictEvent _ = Nothing

toConflictDTO :: KnowledgeModel -> KnowledgeModelEvent -> KnowledgeModelMigrationConflictDTO
toConflictDTO km event =
  KnowledgeModelMigrationConflictDTO
    { eventUuid = event.uuid
    , event = toEventChangeDTO event
    , before = toEntityNodes km conflictQuery [affectedUuid]
    , after = toEntityNodes nextKm conflictQuery [affectedUuid]
    }
  where
    nextKm = applyEvent km event
    affectedUuid
      | entityExists km event.entityUuid || entityExists nextKm event.entityUuid = event.entityUuid
      | otherwise = event.parentUuid

conflictQuery :: KnowledgeModelEntityNodeQuery
conflictQuery = KnowledgeModelEntityNodeQuery {depth = Just 1, content = True}
