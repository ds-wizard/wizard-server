module Shared.Api.Resource.Project.ProjectReplyJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.ProjectReply
import Shared.Util.Aeson

instance ToJSON Reply where
  toJSON = genericToJSON jsonOptions

instance FromJSON Reply where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON ReplyValue where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ReplyValue where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

-- --------------------------------------------------------------------
instance FromJSON IntegrationReplyType where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON IntegrationReplyType where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
