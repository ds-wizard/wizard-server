module Shared.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON AddChapterEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddChapterEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema AddChapterEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON EditChapterEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditChapterEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema EditChapterEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")
