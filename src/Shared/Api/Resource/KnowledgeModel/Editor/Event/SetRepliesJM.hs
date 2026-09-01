module Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Util.Aeson

instance FromJSON SetRepliesDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetRepliesDTO where
  toJSON = genericToJSON jsonOptions
