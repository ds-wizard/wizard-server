module Shared.Api.Resource.Acl.MemberJM where

import Data.Aeson

import Shared.Api.Resource.Acl.MemberDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON MemberDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON MemberDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance JsonSchema MemberDTO where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "type")
