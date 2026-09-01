module Shared.Api.Resource.Project.ProjectContentChangeJM where

import Data.Aeson

import Shared.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Util.Aeson

instance FromJSON ProjectContentChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectContentChangeDTO where
  toJSON = genericToJSON jsonOptions
