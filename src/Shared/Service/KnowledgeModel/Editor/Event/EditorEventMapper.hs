module Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper where

import Data.Aeson (toJSON)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent

toEventChangeDTO :: KnowledgeModelEvent -> KnowledgeModelEditorEventChangeDTO
toEventChangeDTO event =
  KnowledgeModelEditorEventChangeDTO
    { parentUuid = U.toString event.parentUuid
    , entityUuid = U.toString event.entityUuid
    , content = toJSON event.content
    , position = Nothing
    }

fromEventChange :: U.UUID -> U.UUID -> KnowledgeModelEventData -> KnowledgeModelEvent
fromEventChange parentUuid entityUuid content =
  KnowledgeModelEvent
    { uuid = U.nil
    , parentUuid = parentUuid
    , entityUuid = entityUuid
    , content = content
    , createdAt = UTCTime (fromGregorian 1970 1 1) 0
    }

withEventIdentity :: U.UUID -> UTCTime -> KnowledgeModelEvent -> KnowledgeModelEvent
withEventIdentity uuid createdAt event =
  KnowledgeModelEvent
    { uuid = uuid
    , parentUuid = event.parentUuid
    , entityUuid = event.entityUuid
    , content = event.content
    , createdAt = createdAt
    }

toEventsDTO :: Maybe U.UUID -> M.Map String U.UUID -> [KnowledgeModelEntityNodeDTO] -> KnowledgeModelEditorEventsDTO
toEventsDTO revision names outline =
  KnowledgeModelEditorEventsDTO
    { revision = revision
    , names = names
    , outline = outline
    }
