module Shared.Model.KnowledgeModel.Event.KnowledgeModelEventUtil (
  module Shared.Model.KnowledgeModel.Event.Common.CommonUtil,
) where

import Shared.Model.KnowledgeModel.Event.Answer.AnswerEventUtil ()
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEventUtil ()
import Shared.Model.KnowledgeModel.Event.Choice.ChoiceEventUtil ()
import Shared.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEventUtil ()
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Question.QuestionEventUtil ()

instance IsEmptyEvent KnowledgeModelEventData where
  isEmptyEvent (EditAnswerEvent' event) = isEmptyEvent event
  isEmptyEvent (EditChapterEvent' event) = isEmptyEvent event
  isEmptyEvent (EditChoiceEvent' event) = isEmptyEvent event
  isEmptyEvent (EditExpertEvent' event) = isEmptyEvent event
  isEmptyEvent (EditQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent _ = False
