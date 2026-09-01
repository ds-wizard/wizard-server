module Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Util.Aeson

instance ToJSON KnowledgeModelLocaleList where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelLocaleList where
  parseJSON = genericParseJSON jsonOptions
