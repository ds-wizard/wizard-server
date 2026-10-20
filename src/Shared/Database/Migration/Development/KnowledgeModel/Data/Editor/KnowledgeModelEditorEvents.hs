module Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents where

import qualified Data.Map.Strict as M

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventMapper
import Shared.Util.Uuid

knowledgeModelEditorWebsocketEvent1' :: KnowledgeModelEditorWebSocketEventDTO
knowledgeModelEditorWebsocketEvent1' = AddKnowledgeModelEditorWebSocketEventDTO' knowledgeModelEditorWebsocketEvent1

knowledgeModelEditorWebsocketEvent1 :: AddKnowledgeModelEditorWebSocketEventDTO
knowledgeModelEditorWebsocketEvent1 =
  AddKnowledgeModelEditorWebSocketEventDTO
    { uuid = u' "6858b0b6-bb6f-4e21-a0c2-6afc84950f7a"
    , event = a_km1_ch4
    }

setRepliesDTO :: SetRepliesDTO
setRepliesDTO =
  SetRepliesDTO
    { uuid = u' "91863e00-ae98-4bcf-aae6-03a5a801b4fa"
    , replies = fReplies
    }

amsterdamKnowledgeModelEditorEventsChange :: KnowledgeModelEditorEventsChangeDTO
amsterdamKnowledgeModelEditorEventsChange =
  KnowledgeModelEditorEventsChangeDTO
    { events = fmap toEventChangeDTO [a_km1_ch4, a_km1_ch1_q2_eIsaac, m_km1_ch1_q1__to_ch2]
    , baseRevision = Nothing
    , dryRun = Nothing
    }

amsterdamKnowledgeModelEditorEvents' :: KnowledgeModelEditorEventsDTO
amsterdamKnowledgeModelEditorEvents' =
  KnowledgeModelEditorEventsDTO
    { revision = Just $ u' "3e3a6b3c-0d4b-4b8e-9c55-2f6d1f0a7c21"
    , names = M.empty
    , outline = amsterdamKnowledgeModelEditorEventsOutline
    }

amsterdamKnowledgeModelEditorEventsOutline :: [KnowledgeModelEntityNodeDTO]
amsterdamKnowledgeModelEditorEventsOutline =
  [ KnowledgeModelEntityNodeDTO
      { uuid = chapter4WithoutQuestions.uuid
      , parentUuid = Just km1.uuid
      , entityType = ChapterEntityKind
      , title = chapter4WithoutQuestions.title
      , depth = 0
      , content = Nothing
      }
  , KnowledgeModelEntityNodeDTO
      { uuid = km1_ch1_q2_eIsaac.uuid
      , parentUuid = Just question2.uuid
      , entityType = ExpertEntityKind
      , title = km1_ch1_q2_eIsaac.name
      , depth = 0
      , content = Nothing
      }
  , KnowledgeModelEntityNodeDTO
      { uuid = question1.uuid
      , parentUuid = Just chapter2.uuid
      , entityType = QuestionEntityKind
      , title = question1.title
      , depth = 0
      , content = Nothing
      }
  ]
