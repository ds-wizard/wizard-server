module Shared.Service.KnowledgeModel.Editor.EditorMapper where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Shared.Constant.KnowledgeModel
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.ProjectReply
import qualified Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as KMP
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper

toList :: KnowledgeModelEditor -> Maybe KnowledgeModelPackageSuggestion -> KnowledgeModelEditorState -> KnowledgeModelEditorList
toList editor mForkOfPackage state =
  KnowledgeModelEditorList
    { uuid = editor.uuid
    , name = editor.name
    , kmId = editor.kmId
    , version = editor.version
    , state = state
    , previousPackageUuid = editor.previousPackageUuid
    , forkOfPackage = mForkOfPackage
    , createdBy = editor.createdBy
    , createdAt = editor.createdAt
    , updatedAt = editor.updatedAt
    }

toDetailDTO :: KnowledgeModelEditor -> [KnowledgeModelEditorEvent] -> [KnowledgeModelEditorReply] -> Maybe KnowledgeModelPackage -> KnowledgeModel -> Maybe Coordinate -> Maybe KnowledgeModelPackage -> KnowledgeModelEditorState -> KnowledgeModelEditorDetailDTO
toDetailDTO editor kmEditorEvents kmEditorReplies mPreviousPackage knowledgeModel mForkOfPackageId mForkOfPackage state =
  KnowledgeModelEditorDetailDTO
    { uuid = editor.uuid
    , name = editor.name
    , kmId = editor.kmId
    , version = editor.version
    , description = editor.description
    , readme = editor.readme
    , license = editor.license
    , language = editor.language
    , state = state
    , previousPackage = fmap KMP.toSimple mPreviousPackage
    , forkOfPackage = fmap toSimpleDTO mForkOfPackage
    , events = fmap (\KnowledgeModelEditorEvent {..} -> KnowledgeModelEvent {..}) kmEditorEvents
    , replies = M.fromList . fmap (\KnowledgeModelEditorReply {..} -> (path, Reply {..})) $ kmEditorReplies
    , knowledgeModel = knowledgeModel
    , createdBy = editor.createdBy
    , createdAt = editor.createdAt
    , updatedAt = maximum (editor.updatedAt : fmap (.createdAt) kmEditorEvents)
    }

fromCreateDTO :: KnowledgeModelEditorCreateDTO -> U.UUID -> Maybe KnowledgeModelPackage -> U.UUID -> U.UUID -> UTCTime -> KnowledgeModelEditor
fromCreateDTO dto uuid mPreviousPkg createdBy tenantUuid now =
  KnowledgeModelEditor
    { uuid = uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = maybe "" (.description) mPreviousPkg
    , readme = maybe "" (.readme) mPreviousPkg
    , license = maybe "" (.license) mPreviousPkg
    , language = fromMaybe (maybe "en" (.language) mPreviousPkg) dto.language
    , previousPackageUuid = dto.previousPackageUuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = Just createdBy
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: KnowledgeModelEditorChangeDTO -> KnowledgeModelEditor -> UTCTime -> KnowledgeModelEditor
fromChangeDTO dto editor bUpdatedAt =
  KnowledgeModelEditor
    { uuid = editor.uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , language = dto.language
    , previousPackageUuid = editor.previousPackageUuid
    , metamodelVersion = editor.metamodelVersion
    , squashed = editor.squashed
    , createdBy = editor.createdBy
    , tenantUuid = editor.tenantUuid
    , createdAt = editor.createdAt
    , updatedAt = bUpdatedAt
    }

toKnowledgeModelEditorEvent :: U.UUID -> U.UUID -> KnowledgeModelEvent -> KnowledgeModelEditorEvent
toKnowledgeModelEditorEvent knowledgeModelEditorUuid tenantUuid KnowledgeModelEvent {..} = KnowledgeModelEditorEvent {..}

toKnowledgeModelEditorRawEvent :: U.UUID -> U.UUID -> KnowledgeModelRawEvent -> KnowledgeModelEditorRawEvent
toKnowledgeModelEditorRawEvent knowledgeModelEditorUuid tenantUuid KnowledgeModelRawEvent {..} = KnowledgeModelEditorRawEvent {..}

toKnowledgeModelEvent :: KnowledgeModelEditorEvent -> KnowledgeModelEvent
toKnowledgeModelEvent KnowledgeModelEditorEvent {..} = KnowledgeModelEvent {..}

toKnowledgeModelRawEvent :: KnowledgeModelEditorRawEvent -> KnowledgeModelRawEvent
toKnowledgeModelRawEvent KnowledgeModelEditorRawEvent {..} = KnowledgeModelRawEvent {..}

toReplies :: [KnowledgeModelEditorReply] -> M.Map String Reply
toReplies = M.fromList . fmap toReply

toReply :: KnowledgeModelEditorReply -> (String, Reply)
toReply KnowledgeModelEditorReply {..} = (path, Reply {..})
