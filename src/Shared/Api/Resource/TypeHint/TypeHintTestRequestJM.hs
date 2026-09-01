module Shared.Api.Resource.TypeHint.TypeHintTestRequestJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Util.Aeson

instance FromJSON TypeHintTestRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintTestRequestDTO where
  toJSON = genericToJSON jsonOptions
