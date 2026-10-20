module Shared.Database.Migration.Development.Project.Data.ProjectEventBatches where

import qualified Data.UUID as U

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.ProjectLabels
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.ProjectUtil
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Util.Date
import Shared.Util.Uuid

batchEvents :: [ProjectEventChangeDTO]
batchEvents =
  [ SetReplyEventChangeDTO' srche_rQ1
  , SetReplyEventChangeDTO' srche_rQ2
  , SetReplyEventChangeDTO' srche_rQ2_aYes_fuQ1
  , SetReplyEventChangeDTO' srche_rQ4
  , SetReplyEventChangeDTO' srche_rQ4_it3_q6
  , SetReplyEventChangeDTO' srche_rQ10
  , SetReplyEventChangeDTO' srche_rQ13
  , SetReplyEventChangeDTO' srche_rQ14
  , ClearReplyEventChangeDTO' crche_rQ9
  , SetLabelsEventChangeDTO' slblche_rQ2
  , SetPhaseEventChangeDTO' sphsche_2
  ]

rQ4_it3 :: U.UUID
rQ4_it3 = u' "5c0f8b3e-8a51-4d1e-9a39-7f5ad2b1c6e4"

srche_rQ1 :: SetReplyEventChangeDTO
srche_rQ1 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f001"
    , path = createReplyKey [chapter1.uuid, question1.uuid]
    , value = StringReply {sValue = "0000-0002-1825-0097"}
    }

srche_rQ2 :: SetReplyEventChangeDTO
srche_rQ2 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f002"
    , path = createReplyKey [chapter1.uuid, question2.uuid]
    , value = AnswerReply {aValue = q2_answerYes.uuid}
    }

srche_rQ2_aYes_fuQ1 :: SetReplyEventChangeDTO
srche_rQ2_aYes_fuQ1 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f003"
    , path = createReplyKey [chapter1.uuid, question2.uuid, q2_answerYes.uuid, q2_aYes_fuQuestion1.uuid]
    , value = AnswerReply {aValue = q2_aYes_fuq1_answerYes.uuid}
    }

srche_rQ4 :: SetReplyEventChangeDTO
srche_rQ4 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f004"
    , path = createReplyKey [chapter2.uuid, question4.uuid]
    , value = ItemListReply {ilValue = [rQ4_it1, rQ4_it2, rQ4_it3]}
    }

srche_rQ4_it3_q6 :: SetReplyEventChangeDTO
srche_rQ4_it3_q6 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f005"
    , path = createReplyKey [chapter2.uuid, question4.uuid, rQ4_it3, q4_it1_question6.uuid]
    , value = AnswerReply {aValue = q4_it1_q6_answerYes.uuid}
    }

srche_rQ10 :: SetReplyEventChangeDTO
srche_rQ10 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f006"
    , path = createReplyKey [chapter3.uuid, question11.uuid]
    , value = MultiChoiceReply {mcValue = [q11_choice1.uuid, q11_choice2.uuid]}
    }

srche_rQ13 :: SetReplyEventChangeDTO
srche_rQ13 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f007"
    , path = createReplyKey [chapter3.uuid, question13.uuid]
    , value = ItemSelectReply {isValue = rQ4_it3}
    }

srche_rQ14 :: SetReplyEventChangeDTO
srche_rQ14 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f008"
    , path = createReplyKey [chapter3.uuid, question14.uuid]
    , value = FileReply {fValue = projectFile.uuid}
    }

srche_rQ9 :: SetReplyEventChangeDTO
srche_rQ9 =
  SetReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f009"
    , path = createReplyKey [chapter3.uuid, question9.uuid]
    , value = StringReply {sValue = ""}
    }

crche_rQ9 :: ClearReplyEventChangeDTO
crche_rQ9 =
  ClearReplyEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f010"
    , path = createReplyKey [chapter3.uuid, question9.uuid]
    }

slblche_rQ2 :: SetLabelsEventChangeDTO
slblche_rQ2 =
  SetLabelsEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f011"
    , path = createReplyKey [chapter1.uuid, question2.uuid]
    , value = [fLabel1]
    }

sphsche_2 :: SetPhaseEventChangeDTO
sphsche_2 =
  SetPhaseEventChangeDTO
    { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f012"
    , phaseUuid = Just phase2.uuid
    }

q9AsValueQuestionEvent :: QuestionValueType -> [QuestionValidation] -> KnowledgeModelPackageEvent
q9AsValueQuestionEvent valueType validations =
  toPackageEvent germanyKmPackage.uuid U.nil $
    KnowledgeModelEvent
      { uuid = u' "0b1c6f7e-3c55-4c6b-8b0e-2a4d93a1f099"
      , parentUuid = chapter3.uuid
      , entityUuid = question9.uuid
      , content =
          EditQuestionEvent' . EditValueQuestionEvent' $
            EditValueQuestionEvent
              { title = NothingChanged
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = NothingChanged
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , valueType = ChangedValue valueType
              , validations = ChangedValue validations
              }
      , createdAt = dt' 2019 1 1
      }
