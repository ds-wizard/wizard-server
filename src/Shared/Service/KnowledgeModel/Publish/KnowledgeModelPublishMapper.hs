module Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

fromPackage
  :: KnowledgeModelEditor
  -> U.UUID
  -> Maybe U.UUID
  -> Maybe Coordinate
  -> Maybe Coordinate
  -> String
  -> String
  -> String
  -> [KnowledgeModelEvent]
  -> UTCTime
  -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromPackage editor uuid workspaceUuid forkOfPkgId mergeCheckpointPkgId version description readme events now =
  ( KnowledgeModelPackage
      { uuid = uuid
      , name = editor.name
      , id = editor.id
      , version = version
      , phase = ReleasedKnowledgeModelPackagePhase
      , metamodelVersion = knowledgeModelMetamodelVersion
      , description = description
      , readme = readme
      , license = editor.license
      , language = editor.language
      , previousPackageUuid = editor.previousPackageUuid
      , forkOfPackageId = forkOfPkgId
      , mergeCheckpointPackageId = mergeCheckpointPkgId
      , nonEditable = False
      , public = False
      , tenantUuid = editor.tenantUuid
      , workspaceUuid = workspaceUuid
      , createdAt = now
      }
  , fmap (toPackageEvent uuid editor.tenantUuid) events
  )
