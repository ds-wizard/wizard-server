module Shared.Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Report.Report
import Shared.Util.Aeson
import Shared.Util.JsonSchema

-- --------------------------------------------------------------------
instance FromJSON Report where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Report where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON TotalReport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TotalReport where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON ChapterReport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ChapterReport where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON MetricSummary where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MetricSummary where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance ToJSON Indication where
  toJSON = toSumJSONWithTypeField "indicationType" ""

instance FromJSON Indication where
  parseJSON (Object o) = do
    indicationType <- o .: "indicationType"
    case indicationType of
      "AnsweredIndication" -> parseJSON (Object o) >>= \event -> return (AnsweredIndication' event)
      "PhasesAnsweredIndication" -> parseJSON (Object o) >>= \event -> return (PhasesAnsweredIndication' event)
      _ -> fail "One of the references has unsupported indicationType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON AnsweredIndication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AnsweredIndication where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON PhasesAnsweredIndication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PhasesAnsweredIndication where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance JsonSchema TotalReport where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema MetricSummary where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema Indication where
  jsonSchema _ =
    anyOfSchema
      [ withConstProperties [("indicationType", "AnsweredIndication")] (jsonSchema (Proxy :: Proxy AnsweredIndication))
      , withConstProperties [("indicationType", "PhasesAnsweredIndication")] (jsonSchema (Proxy :: Proxy PhasesAnsweredIndication))
      ]

instance JsonSchema AnsweredIndication where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema PhasesAnsweredIndication where
  jsonSchema = genericJsonSchema jsonOptions
