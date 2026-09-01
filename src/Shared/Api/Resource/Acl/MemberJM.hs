module Shared.Api.Resource.Acl.MemberJM where

import Data.Aeson

import Shared.Api.Resource.Acl.MemberDTO
import Shared.Util.Aeson

instance FromJSON MemberDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON MemberDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
