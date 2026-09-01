module Shared.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Util.Swagger

instance ToSchema AddChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1

instance ToSchema EditChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1

instance ToSchema DeleteChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1
