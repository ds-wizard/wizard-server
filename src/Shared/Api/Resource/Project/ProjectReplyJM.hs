module Shared.Api.Resource.Project.ProjectReplyJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.ProjectReply
import Shared.Util.Aeson
import Shared.Util.JsonSchema

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

instance JsonSchema Reply where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema ReplyValue where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "type")

instance JsonSchema IntegrationReplyType where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "type")
